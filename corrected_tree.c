#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>


#include "util.h"
#include "corrected_binomial-tree.h"
#include "corrected_binomial-tree_io.h"
#include "corrected_lame-tree.h"
#include "corrected_gossip.h"



#if CORRT_IMPL_INSIDE_OMPI
    // types
    #define CORRT_MPI_STATUS ompi_status_public_t
    #define CORRT_OMPI_MODULE_TYPE mca_coll_base_module_t

    // functions
    #define CORRT_ALLOC_REQS(NUM) \
        coll_base_comm_get_reqs(module->base_data, NUM)

    #define CORRT_MPI_ABORT(A,B) \
        ompi_mpi_abort(A,B)
    #define CORRT_MPI_COMM_SIZE(COMM,SIZE) \
        *(SIZE) = ompi_comm_size(COMM)
    #define CORRT_MPI_COMM_RANK(COMM,RANK) \
        *(RANK) = ompi_comm_rank(COMM)

    #define CORRT_MPI_ISEND(A,B,C,D,E,F,G) \
        MCA_PML_CALL(isend(A,B,C,D,E,MCA_PML_BASE_SEND_STANDARD,F,G))
    #define CORRT_MPI_SEND(A,B,C,D,E,F) \
        MCA_PML_CALL(send(A,B,C,D,E,MCA_PML_BASE_SEND_STANDARD,F))
    #define CORRT_MPI_RECV_INIT(A,B,C,D,E,F,G) \
        MCA_PML_CALL(irecv_init(A,B,C,D,E,F,G))
    #define CORRT_MPI_START(A) \
        MCA_PML_CALL(start(1, A))

    #define CORRT_MPI_TESTSOME(A,B,C,D,E) \
        ompi_request_test_some(A,B,C,D,E)
    #define CORRT_MPI_WAITSOME(A,B,C,D,E) \
        ompi_request_wait_some(A,B,C,D,E)
    #define CORRT_MPI_WAITALL(A,B,C,D,E,F,G,H) \
        ompi_request_wait_all(A,B,C,D,E,F,G,H)

    #include "corrected_binomial-tree.c"
    #include "corrected_binomial-tree_io.c"
    #include "corrected_lame-tree.c"
    #include "corrected_gossip.c"
    #include "util.c"

#else
    #include <mpi.h>

    // (additional) constants
    static int const MCA_COLL_BASE_TAG_BCAST = 2342; // message tag to use for our bcast

    // types
    #define CORRT_MPI_STATUS MPI_Status
    #define CORRT_OMPI_MODULE_TYPE void

    // functions
    #define CORRT_ALLOC_REQS(NUM) \
        malloc( (NUM) * sizeof(MPI_Request) )

    #define CORRT_MPI_ABORT(A,B) \
        PMPI_Abort(A,B)
    #define CORRT_MPI_COMM_SIZE(COMM,SIZE) \
        PMPI_Comm_size(COMM,SIZE)
    #define CORRT_MPI_COMM_RANK(COMM,RANK) \
        PMPI_Comm_rank(COMM,RANK)

    #define CORRT_MPI_ISEND(A,B,C,D,E,F,G) \
        PMPI_Isend(A,B,C,D,E,F,G)
    #define CORRT_MPI_SEND(A,B,C,D,E,F) \
        PMPI_Send(A,B,C,D,E,F)
    #define CORRT_MPI_RECV_INIT(A,B,C,D,E,F,G) \
        PMPI_Recv_init(A,B,C,D,E,F,G)
    #define CORRT_MPI_START(A) \
        PMPI_Start(A)

    #define CORRT_MPI_TESTSOME(A,B,C,D,E) \
        PMPI_Testsome(A,B,C,D,E)
    #define CORRT_MPI_WAITSOME(A,B,C,D,E) \
        PMPI_Waitsome(A,B,C,D,E)
    #define CORRT_MPI_WAITALL(A,B,C,D,E,F,G,H) \
        PMPI_Waitall(A,B,C,D,E,F,G,H)
#endif


/******************************************************************************
 * Global buffers or values                                                   *
 *                                                                            *
 * After initialisation (during the first bcast) these remain constant and    *
 * are available/used across all following functions.                         *
 *                                                                            *
 * Whereas small values are stored directly, we allocate large buffers for    *
 * those whose size is not known at compile time. These are pointers to       *
 * 'malloc'ed memory.                                                         *
 ******************************************************************************/
static size_t corr_dist  = 0xBEEFBABE, // opportunistic correction distance (mark as uninit)
              count_max  = 0xBEEFBABE; // maximum allowed message size (mark as uninit)

static size_t const epoch_wrap = 128; // value at which the local epoch wraps

/* Request objects for sends and receives; see also 'req_*' below for
 * more info about the structure */
static MPI_Request *reqs = NULL;

/* Dissemination graph structure */
static size_t num_child  = 0xBEEFBABE, // number of child ranks
              num_parent = 0xBEEFBABE, // number of parent ranks (1 for trees)
              *parents   = NULL,       // array of parent ranks
              *children  = NULL;       // array of child ranks



/* Dedicated buffers for each of our receives; 'count_max' entries each
 *
 * [0          .. num_parent - 1] -> dissemination message from parents
 * [num_parent .. corr_dist]      -> correction from 'corr_dist' left neighbours
 */
static char *buffers = NULL;

/* Principle structure of the 'reqs' array of request objects:
 *
 * => 'req_diss_rcv' refers to the next (start) index, i.e. '0'
 * [0 .. ] -> recv from parents
 *
 * => 'req_corr_rcv' refers to the next (start) index
 * [num_parent .. ] -> recv correction from right neighbours
 *
 * => 'req_diss_snd' refers to the next (start) index
 * [corr_dist + num_parent .. ] -> send to children in diss graph
 *
 * => 'req_corr_snd' refers to the next (start) index
 * [corr_dist + num_child + num_parent ..] -> send correction to left neighbours
 *
 *
 * For receives, which are persistent requests, the same indices are also
 * used for the corresponding receive 'buffers'. Sending simply uses the
 * user-provided 'buff' after it has been filled with the correct data. Thus
 * the send buffer may change and we cannot use persistent requests there.
 *
 * Note that some request might not be used for individual ranks (e.g.
 * sending fewer corrections because the node is at the end of the chain or
 * due to optimisation), but the structure discussed above is *always*
 * maintained. Unused array elements will contain 'REQUEST_NULL' and thus
 * not interfere.
 */
size_t req_diss_rcv = 0,
       req_corr_rcv = 0xBEEFBABE,
       req_diss_snd = 0xBEEFBABE,
       req_corr_snd = 0xBEEFBABE,
       req_past_end = 0xBEEFBABE;


// Current epoch or broadcast round
typedef unsigned int epoch_t;
static epoch_t epoch_global = 0;


/* We keep the absolute epoch our correction partners are in.
 *
 * These values might not be accurate but rather a lower bound because
 * correction messages are optimised away. For trees, the dissemination
 * messages will always be sent but Gossip may sometimes skip some children
 * depending on the Gossip round we get a message from.
 *
 * The indices are consistent with those in 'buffers'.
 */
static epoch_t *epoch_neigh = NULL;

/* Does the respective rank's entry in 'buffers' contain valid data?
 *
 * This is the case if we already received a "future" broadcast from
 * a faster rank. (Only!) Then the corresponding rcv operation in 'reqs'
 * will be inactive.
 */
static bool *buff_fut = NULL;


/* Gossip-specific values
 *
 * In our simplified Gossip we assign 'gossip_rounds' children to each node.
 * However, most nodes will not actually send to all of them. So we need to
 * keep the number of children we did send to for correction optimisation.
 */
static size_t num_child_diss;

// If we perform Gossip, keep the number of rounds to do
static int gossip_rounds = 0xDEADABE;



/* Arrays used for 'Testsome'/'Waitsome' ('static' to avoid stack allocation) */
static CORRT_MPI_STATUS *statuses = NULL;
static int              *indices  = NULL;

static int size, rank; // We only support COMM_WORLD, so these will be fixed


/******************************************************************************
 * Statistics                                                                 *
 ******************************************************************************/
static void statistics_count_future(bool b);
static void statistics_count_skipped(int a, bool b);
void corrt_statistics_print(void);

#ifndef CORRT_DO_STATISTICS
static void statistics_count_future(bool b) {}
static void statistics_count_skipped(int a, bool b) {}
void corrt_statistics_print() {}
#else
static size_t future[2] = {0,0}; // [0] = diss, [1] = corr
static size_t skipped[2][3] = {{9423,0,0},  // [0][*] = regular, [1][*] = future
                               {9423,0,0}}; // [*][0] = min, [*][1] = max, [*][2] = sum (for avg)

static void statistics_count_future(bool is_corr) {
    ++future[is_corr];
}

static void statistics_count_skipped(int dist_saved, bool is_future) {
    assert(dist_saved > 0 && "Nothing gained. Don't bother me.");
    if (dist_saved < skipped[is_future][0]) { skipped[is_future][0] = dist_saved; }
    if (dist_saved > skipped[is_future][1]) { skipped[is_future][1] = dist_saved; }
    skipped[is_future][2] += dist_saved;
}

void corrt_statistics_print() {
    assert(MPI_SUCCESS == 0);
    // Evil and hacky ... but should do the jobs
    if (rank) {
        if (PMPI_Reduce(&(skipped[0][0]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, 0, MPI_COMM_WORLD)
          + PMPI_Reduce(&(skipped[1][0]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, 0, MPI_COMM_WORLD)
          + PMPI_Reduce(&(skipped[0][1]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, 0, MPI_COMM_WORLD)
          + PMPI_Reduce(&(skipped[1][1]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, 0, MPI_COMM_WORLD)
          + PMPI_Reduce(&(skipped[0][2]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD)
          + PMPI_Reduce(&(skipped[1][2]), NULL, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD)
          != MPI_SUCCESS) {
            fprintf(stderr, "Reduce failed");
        }
        return; // let only root print
    }

    if (PMPI_Reduce(MPI_IN_PLACE, &(skipped[0][0]), 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, 0, MPI_COMM_WORLD)
      + PMPI_Reduce(MPI_IN_PLACE, &(skipped[1][0]), 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, 0, MPI_COMM_WORLD)
      + PMPI_Reduce(MPI_IN_PLACE, &(skipped[0][1]), 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, 0, MPI_COMM_WORLD)
      + PMPI_Reduce(MPI_IN_PLACE, &(skipped[1][1]), 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, 0, MPI_COMM_WORLD)
      + PMPI_Reduce(MPI_IN_PLACE, &(skipped[0][2]), 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD)
      + PMPI_Reduce(MPI_IN_PLACE, &(skipped[1][2]), 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD)
      != MPI_SUCCESS) {
        fprintf(stderr, "Reduce failed");
        return;
    }

    fprintf(stderr,
            "Total number of future messages: %zd dissemination, %zd correction\n"
            "Correction messages saved (regular): %zd (min), %zd (max), %f (avg)\n"
            "Correction messages saved (future):  %zd (min), %zd (max), %f (avg)\n\n",
            future[0], future[1],
            (skipped[0][0] == 9423 ? 0 : skipped[0][0]), skipped[0][1], (skipped[0][2]/(double)epoch_global)/size,
            (skipped[1][0] == 9423 ? 0 : skipped[1][0]), skipped[1][1], (skipped[1][2]/(double)epoch_global)/size);
}
#endif



/******************************************************************************
 * Generic helper functions                                                   *
 ******************************************************************************/

/*
 * Initialise all relevant diss graph parameters for this rank (pointers).
 * The graph is usually a tree but for Gossip we also allow a more general
 * graph with multiple parents.
 *
 * The graph type is read from the environment.
 *
 * Returns MPI status code
 */
static inline int
setup_diss_graph(int const rank, int const comm_size)
{
    assert(rank >= 0 && "Invalid rank");
    assert(comm_size > 0 && "Invalid size");
    assert(num_child  == 0xBEEFBABE &&
           num_parent == 0xBEEFBABE &&
           parents  == NULL &&
           children == NULL && "Graph already initialised");
	 assert(gossip_rounds == 0xDEADABE && "Gossip rounds already initialised");

    char const * const graph_type = read_env_or_fail("CORRT_DISS_TYPE");

    // generic for all trees -> single parent, no Gossip
    if (0 == strncmp(graph_type, "tree_", 5)) {
        num_parent = 1;
        parents    = malloc( sizeof(size_t) );
        if (!parents) { return CORRT_ERR_NO_MEM; }

        gossip_rounds = -1;
    }

    if (0 == strncmp(graph_type, "tree_binomial", 13)) {
        return setup_tree_binomial(rank, comm_size, &num_child, parents, &children);
    }
    if (0 == strncmp(graph_type, "tree_binomial_in_order", 22)) {
        return setup_tree_binomial_in_order(rank, comm_size, &num_child, parents, &children);
    }
    if (0 == strncmp(graph_type, "tree_lame", 9)) {
        return setup_tree_lame(rank, comm_size, &num_child, parents, &children);
    }
    if (0 == strncmp(graph_type, "gossip", 6)) {
        return setup_gossip(rank, comm_size, &gossip_rounds, &num_child, &num_parent, &parents, &children);
    }

    fprintf(stderr, "Unknown dissemination: '%s'\n"
                    "See function '%s' near line %d in file '%s' for valid values)\n",
            graph_type, __func__, __LINE__, __FILE__);
    CORRT_MPI_ABORT(MPI_COMM_WORLD, -1);
    return CORRT_ERR_NOT_IMPL; // unreached
}

/* Test whether 'ele' is in the 'list' array which has length 'len' */
static inline bool
is_member(size_t const ele,
          size_t const len,
          size_t const * const list)
{
    assert( (!len || list) && "Broken list");
    for (size_t cc = 0; cc < len; ++cc) {
        if (list[cc] == ele) { return true; }
    }
    return false;
}

/* Test whether 'rank' is one of our children (i.e. is in the 'children' array)
 *
 * Note: Do not consider children skipped in dissemination! (only relevant for Gossip)
 */
static inline bool
is_child(size_t const rank)
{
    assert(!is_member(0, num_child, children) && "Root is this node's child?!");
    return is_member(rank, num_child_diss, children);
}

/* Test whether 'rank' is one of our parents (i.e. is in the 'parents' array) */
static inline bool
is_parent(size_t const rank)
{
    return is_member(rank, num_parent, parents);
}


/* Many of our buffers are dependent on various parameters, so they're
 * allocated before/during the first bcast. Nevertheless, they are available as
 * global variables because most functions need to access them. Here we allocate
 * those buffers.
 *
 * return MPI status code
 */
static inline int
allocate_buffers(CORRT_OMPI_MODULE_TYPE *module)
{
    assert(!reqs && !epoch_neigh && !buff_fut && !buffers && !statuses && !indices && "Memory already allocated!?");

    // TODO: If any ressource allocation fails, we just leak previously
    //       allocated ressources. Same for later communication problems ...
    //       prototype, you know ;-)

    // Dedicated buffers for each of the receives; 'count_max' entries each
    buffers = malloc( (num_parent + corr_dist) * count_max * sizeof(char) );
    if (!buffers) { return CORRT_ERR_NO_MEM; }

    /* For each of our parents and "correction neighbours" we store the
     * (absolute) epoch in which we expect to get the next message from
     * them; initially that's '0'.
     * If there has already been a "future message", we remember the epoch
     * of that message, which is held in the neighbour's rcv buffer and note
     * that there's pending future message via 'buff_fut'.
     */
    epoch_neigh = calloc(num_parent + corr_dist, sizeof(epoch_t));
    if (!epoch_neigh) { return CORRT_ERR_NO_MEM; }

    // Flags to keep track of already-received future messages ('0'/'false' -> no future message received)
    buff_fut = calloc(num_parent + corr_dist, sizeof(bool));
    if (!buff_fut) { return CORRT_ERR_NO_MEM; }

    /* Request objects for all sends + receives (init to 'MPI_REQUEST_NULL'!) */
    reqs = CORRT_ALLOC_REQS(num_parent + num_child + 2 * corr_dist);
    if (!reqs) { return CORRT_ERR_NO_MEM; }

    for (size_t cc=0; cc < num_parent + num_child + 2 * corr_dist; ++cc) {
        reqs[cc] = MPI_REQUEST_NULL;
    }

    /* Arrays for 'Waitsome' */
    statuses  = malloc( (num_parent + num_child + 2 * corr_dist) * sizeof(CORRT_MPI_STATUS) );
    if (!statuses) { return CORRT_ERR_NO_MEM; }
    indices = malloc( (num_parent + num_child + 2 * corr_dist) * sizeof(int) );
    if (!indices) { return CORRT_ERR_NO_MEM; }

    assert(reqs && epoch_neigh && buff_fut && buffers && statuses && indices && "Memory not properly allocated.");
    return MPI_SUCCESS;
}


#if 0
// not used anymore since Gossip forces us to send full epochs

/* Check received message and (potentially) update our view of the sender's epoch
 *
 * returns 'true' if message is from current epoch,
 *         'false' for stale or future messages
 */
static inline bool
update_epoch_neigh(unsigned long long const        epoch_global,
                   char               const        epoch_ltd,
                   unsigned long long       *const epoch_neigh,
                   char               const *const buffer)
{
    assert(epoch_global % epoch_wrap == (unsigned char) epoch_ltd && "Wrong limited epoch");

    /* Messages (including messages from our parents) might be
     * stale, i.e. still left over from previous epochs. We also need
     * to keep our view on other rank's epochs updated.
     */
    char const epoch_rcvd = buffer[0];
    long long const epoch_hyper_global = epoch_global / epoch_wrap,
                    epoch_hyper_neigh  = *epoch_neigh / epoch_wrap;

    assert(epoch_hyper_global >= epoch_hyper_neigh && "Neighbour is ahead");
    assert((epoch_hyper_global - epoch_hyper_neigh) <= 1 && "Neighbour is behind too far");
    assert((epoch_hyper_global != epoch_hyper_neigh ||
            epoch_ltd >= epoch_rcvd) && "Neighbour is ahead");

    /* For the received epoch to be "correct" it needs to match our
     * limited epoch. However, since there might be *very* old messages
     * we need to make sure that the hyper epoch also matches.
     */
    if (epoch_ltd == epoch_rcvd &&
        epoch_hyper_global == epoch_hyper_neigh) {
        *epoch_neigh = epoch_global; // also update the epoch of our neighbour!
        return true; // perfect match => up-to-date message
    }

    /* Stale or future message -> update the sender's epoch
     *
     * We are only guaranteed to get a (correction) message at least at the
     * start of every hyper epoch.
     */

    /* We need to jump to the start of the next hyper epoch iff we expect an
     * epoch that is *not* at the start of a hyper epoch (first part of the
     * 'if') but receive an epoch number that is (second part).
     */
    if ((*epoch_neigh % epoch_wrap) && !(epoch_rcvd % epoch_wrap)) {
        *epoch_neigh = epoch_hyper_neigh * epoch_wrap + epoch_wrap;
    } else {
        *epoch_neigh = epoch_hyper_neigh * epoch_wrap + epoch_rcvd;
    }

    return *epoch_neigh == epoch_global;
}
#endif

/* Check received message and (potentially) update our view of the sender's epoch
 *
 * returns 'true' if message is from current epoch,
 *         'false' for stale or future messages
 */
static inline bool
update_epoch_neigh(epoch_t       *const epoch_neigh,
                   char    const *const buffer)
{
    /* Messages (including messages from our parents) might be
     * stale, i.e. still left over from previous epochs. We also need
     * to keep our view on other rank's epochs updated.
     */
    epoch_t const epoch_rcvd = ((epoch_t*)buffer)[0];

    // Update the sender's epoch
    *epoch_neigh = epoch_rcvd;

    // For the received epoch to be "correct" it needs to match our epoch.
    return *epoch_neigh == epoch_global;
}


/* Any non-root node must keep receiving until it gets the message for the
 * current epoch. While doing this, we also handle message from past and future
 * epochs.
 *
 * Receive buffers are active iff no future messages was received on them and
 * this property *must* be upheld by this function.
 *
 * return MPI status code
 */
static inline int
receive_initial_message(int     const count,        // IN
                        void   *const buff,         // OUT
                        size_t *const corr_neigh    // IN+OUT
                       )
{
    size_t idx; // index (in basically all the global arrays) of (last) successful rcv request
    bool   done = false; // got a message (for current epoch) yet?
    int    err  = MPI_SUCCESS;

    /* We might already have received a/some "future" message(s) for
     * the current epoch...
     *
     * Note that we need to find *all* "future" messages for this
     * epoch and re-enable the respective receives.
     */
    for (idx = req_diss_rcv; idx < req_diss_snd; ++idx) {
        // Only exact epoch matches are relevant here
        if ( ! (buff_fut[idx] && epoch_neigh[idx] == epoch_global) ) {
            continue;
        }

        statistics_count_future(idx >= req_corr_rcv);

        /* We actually got the right message, store it in the user's buffer.
         * Doing this once is enough however.
         *
         * Note: This *must* be done before we reactivate the rcv! Races...
         */
        if (!done) {
            memcpy(buff, &buffers[idx * count_max], count);
            done = true;
        }

        // For diss messages we are done at this point
        if (idx < req_corr_rcv) { continue; }

        // This message will have an effect on our own correction if its
        // sender was closer to us than any previous sender (in this epoch)
        size_t const dist = idx - req_corr_rcv;
        if (dist < *corr_neigh) {
            statistics_count_skipped(*corr_neigh - dist, true);
            *corr_neigh = dist;
        }

        // Reactivate the receive request, now that we caught up
        err = CORRT_MPI_START(&reqs[idx]);
        if (MPI_SUCCESS != err) { return err; }
    } // end -- handle stored future messages for this epoch


    /* If we don't have the message for this epoch yet, wait for anybody to
     * send it to us: drop stale messages, reactivate recvs, store the
     * payload in the user's buffer once rcv'd and properly handle "future"
     * messages
     */
    while (!done) {
        int completed = MPI_UNDEFINED; // better be safe and initialise it

        /* It's fine to use all recv reqs since irrelevant ones are set to
         * 'MPI_REQUEST_NULL' or inactive and thus will be ignored :-)
         */
        err = PMPI_Waitsome(req_diss_snd - req_diss_rcv, // incount   [IN]
                            &reqs[req_diss_rcv], // array of requests [IN+OUT]
                            &completed,          // outcount          [OUT]
                            indices,             // array of indices  [OUT]
                            statuses);           // array of statuses [OUT]
        if (MPI_SUCCESS != err) { return err; }

        assert(completed != MPI_UNDEFINED && "No active requests!?");

        /* Handle *all* completed requests */
        for (int cc = 0; cc < completed; ++cc) {
            size_t           const    idx = indices[cc];
            CORRT_MPI_STATUS const status = statuses[cc];

            assert(status.MPI_SOURCE >= 0 && "Invalid sender rank");
            assert( (idx < req_corr_rcv || (rank - status.MPI_SOURCE > 0)) && "Correction message from our right");
            assert( (
                     (idx < req_corr_rcv && (unsigned)status.MPI_SOURCE == parents[idx]) || // rcv from parent
                     (rank > status.MPI_SOURCE && (idx - req_corr_rcv + 1) == (size_t)(rank - status.MPI_SOURCE))
                    ) && "Unexpected index for sender");

            /* Note that 'idx' "incidentally" corresponds to the sender rank's
             * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
             */
            bool matches = update_epoch_neigh(&epoch_neigh[idx],
                                              &buffers[idx * count_max]);
            assert(matches == (epoch_neigh[idx] == epoch_global) && "Function broken!?");


            // We actually got the right message, so...
            if (matches) {
                // ... store it in the user's buffer if we didn't have a match before
                if (!done) {
                    memcpy(buff, &buffers[idx * count_max], count);
                    done = true;
                }
                // ... or use the opportunity for a consistency check
                else {
//                     assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
                    //  -> not valid for Gossip ... but we may want to find the Gossip message with the smallest number of rounds left if we have multiple... TODO?
                }

                // Note: The previous *must* be done before we reactivate the send! Races...

                // If this was a correction, it will have an effect on our own
                // correction iff its sender was closer to us than any previous
                // sender (in this epoch)
                if (idx >= req_corr_rcv) {
                    size_t const dist = idx - req_corr_rcv + 1; // distance between sender and us
                                                                // req_corr_rcv is 1 from us,
                                                                // req_corr_rcv + 1 is 2 from us, ...

                    assert(rank < status.MPI_SOURCE && dist == (size_t)(rank - status.MPI_SOURCE) && "Unexpected index for sender");

                    if (dist < *corr_neigh) {
                        statistics_count_skipped(*corr_neigh - dist, false);
                        *corr_neigh = dist;
                    }
                }
            }
            // Future message
            // Note: Nothing special to do for stale messages, just ignore them
            else if (epoch_neigh[idx] > epoch_global) {
                buff_fut[idx] = true;
                continue; // do *not* reactivate the rcv nor increase the epoch
            }

            // Reactivate the completed recv
            err = CORRT_MPI_START(&reqs[idx]);
            if (MPI_SUCCESS != err) { return err; }

            // Next time, we expect the next epoch from him, of course
            ++epoch_neigh[idx];
        } // end -- handle completed requests
    } // end -- still waiting for a message for this epoch

    return err;
}


/*
 * Send correction messages to our 'corr_dist' right neighbours
 *
 * Do not send to our children or to root (implicitly since it's either "to
 * our left" or "beyond the end of the chain" as we don't close the ring).
 *
 * As a (possible) optimisation, we send corrections one by one, omitting
 * corr messages to those ranks that we know will be covered be others
 * (that already sent to us).
 *
 * Example: (R-3) (R-2) (R-1)  (R)  (R+1) (R+2) (R+3)
 *
 * If we (R) received from (R-1) and 'corr_dist' == 3, we know (R-1) will
 * also send corr to (R+1) and (R+2), so we only send to (R+3).
 *
 * Note that we might already have received corr messages before, so
 * consider 'corr_neigh' which gives the distance to the closest rank
 * we received correction from for this epoch.
 *
 * return MPI status code
 */
static inline int
do_correction(int          const count,        // IN
              MPI_Datatype const datatype,     // IN
              MPI_Comm     const comm,         // IN
              void         const *const buff,  // IN
              size_t             corr_neigh    // IN (+modified locally)
)
{
    int    err    = MPI_SUCCESS;
    bool   first  = true; // first round in the correction phase?
    size_t offset = 0;    // offset of node to send to currently
                          // Note: We don't send anything in the first round and
                          // increment this value every round, so effectively we
                          // start with 'offet = 1'.

    while (true) {
        bool done = first; // sent of current corr message done? (no message is being sent in the first round!)

        do {
            int completed = MPI_UNDEFINED; // better be safe and initialise it

            /* First, check/wait for any pending operation to finish. We only
             * want a single corr send in flight.
             *
             * It's fine to use all reqs since irrelevant ones are set to
             * 'MPI_REQUEST_NULL' or inactive and thus will be ignored :-)
             */
            if (first) {
                // do not wait yet, just check if any requests are already done
                err = CORRT_MPI_TESTSOME(req_past_end - req_diss_rcv, // incount
                                         &reqs[req_diss_rcv], // array of requests
                                         &completed,          // outcount
                                         indices,             // array of indices
                                         statuses);           // array of statuses
                first = false;
            }
            else {
                err = CORRT_MPI_WAITSOME(req_past_end - req_diss_rcv, // incount
                                         &reqs[req_diss_rcv], // array of requests
                                         &completed,          // outcount
                                         indices,             // array of indices
                                         statuses);           // array of statuses
            }
//             assert(completed != MPI_UNDEFINED) && "No active requests!?");
//             --> not for Gossip's root node as Gossip sends synchronously

            /* From the MPI standard:
             * "The call will return the error code MPI_ERR_IN_STATUS and the
             *  error field of each status returned will be set to indicate
             *  success or to indicate the specific error that occurred.
             *  The call will return MPI_SUCCESS if no request resulted in an
             *  error, and will return another error code if it failed for other
             *  reasons (such as invalid arguments). In such cases, it will not
             *  update the error fields of the statuses."
             */
            if (MPI_SUCCESS != err && MPI_ERR_IN_STATUS != err) { return err; }

            /* Handle *all* completed requests */
            for (int cc = 0; cc < completed; ++cc) {
                assert(indices[cc] >= 0 && "Test/Waitsome broken!?");

                size_t           const    idx = indices[cc];  // convenience...
                CORRT_MPI_STATUS const status = statuses[cc]; // ...copies :-)

                // send operation finished
                if (idx >= req_diss_snd) {
                    // Was it a (i.e. our current) corr message?
                    // Note: We don't care much about tree sends here...
                    if (idx >= req_corr_snd) {
                        assert(idx == req_corr_snd + offset - 1 && "Unexpected send");
                        done = true;
                    }

                    // That's all, just handle the next completion
                    continue;
                }
                // receive finished
                else {
                    size_t const dist = idx - req_corr_rcv + 1; // distance between sender and us
                                                                // req_corr_rcv is 1 from us,
                                                                // req_corr_rcv + 1 is 2 from us, ...

                    assert( (MPI_SUCCESS == err || MPI_SUCCESS == status.MPI_ERROR) && "Failed recv");
//                     assert(rank > status.MPI_SOURCE && "Correction from right neighbour (or weird/small parent rank)");
//                     -> not with Gossip
                    assert( (idx < req_corr_rcv ||
                             (rank > status.MPI_SOURCE && dist == (size_t)(rank - status.MPI_SOURCE))
                            ) && "Unexpected index for sender");

                    /* Note that 'idx' "incidentally" corresponds to the sender rank's
                     * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
                     */
                    bool matches = update_epoch_neigh(&epoch_neigh[idx],
                                                      &buffers[idx * count_max]);

                    assert(matches == (epoch_neigh[idx] == epoch_global) && "Function broken!?");

                    // It's the right epoch, but we already got the message way before...
                    if (matches) {
                        // ... use the opportunity for a consistency check
                        // Note: This *must* be done before we reactivate the send! Races...
//                         assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
//                         -> not with Gossip

                        // keep track of the closest sender
                        // (unless the message came from our parent)
                        if (idx >= req_corr_rcv && dist < corr_neigh) {
                            statistics_count_skipped(corr_neigh - dist, false);
                            corr_neigh = dist;
                        }
                    }
                    // Future message
                    // Note: Nothing special to do for stale messages
                    else if (epoch_neigh[idx] > epoch_global) {
                        buff_fut[idx] = true;
                        continue; // do *not* reactivate the rcv or increase epoch, handle next completion
                    }
                } // end -- recv

                // Reactivate completed recv
                err = CORRT_MPI_START(&reqs[idx]);
                if (MPI_SUCCESS != err) { return err; }

                // Next time, we expect the next epoch from him, of course
                ++epoch_neigh[idx];
            } // end -- handle completed requests
        } while (!done); // end -- while (current corr message not sent)

        /* Our current corr message has been sent now. Before considering the
         * next corr message, update 'offset' so ranks covered by anybody who
         * sent to us are not bothered by us as well. The other guy (that just
         * sent to us) will reach those faster anyway.
         *
         * Note: 'offset' indicates the distance to the rank we just sent to.
         */
        assert(corr_neigh <= corr_dist && "Optimisation broken");

        size_t const handled_by_sender = corr_dist - corr_neigh;
        offset = (offset > handled_by_sender) ? offset : handled_by_sender;


        int receiver;

        // Find the next receiver, but do not send to children
        do {
            ++offset;
            receiver = rank + offset; // send to right
        } while (is_child(receiver));

        // Handle end of the chain (1st part) or end of correction (2nd part)
        if (receiver >= size || offset > corr_dist) { break; }

        err = CORRT_MPI_ISEND(buff,
                         count,
                         datatype,
                         receiver,
                         MCA_COLL_BASE_TAG_BCAST,
                         comm,
                         &reqs[req_corr_snd + offset - 1]);
        if (MPI_SUCCESS != err) { return err; }

    } // end -- cover 'corr_dist' neighbours with 'offset'


    assert(MPI_SUCCESS == err);
    return err;
}

/*
 * Core implementation of corrected broadcast
 */
int
ompi_coll_base_bcast_intra_corrected(void *const buff,
                                     int const count,
                                     MPI_Datatype const datatype,
                                     int const root,
                                     MPI_Comm const comm,
                                     CORRT_OMPI_MODULE_TYPE *module)
{
    if (0 == count) { return MPI_SUCCESS; } // that was simple :-)

    /* Reject unexpected parameters ... it's only a prototype after all
     *
     * Also note that we're "stealing" the first 'char' in the user data to
     * trasmit the epoch. In a real implementation this would instead be part
     * of OMPI's internal metadata that establishes message order anyways.
     */
    if (root != 0 ||
        datatype != MPI_CHAR ||
        count < 0 ||
        (unsigned)count < (sizeof(epoch_t) + (gossip_rounds >= 0 ? sizeof(char) : 0)) ||
        comm != MPI_COMM_WORLD) {

        fprintf(stderr, "Unsupported parameters\n");
        return CORRT_ERR_NOT_IMPL;
    }

    int err = MPI_SUCCESS;


    /* On the first call to this function, i.e. the very first bcast, prepare
     * basic information for the tree to be used. This needs to be done before
     * we split up the buffers in the next step.
     */
    if (epoch_global == 0) {
        // our rank number and the comm size should stay constant, so fetch them here just once
        assert(!rank && !size && "MPI info already set!?");
        CORRT_MPI_COMM_SIZE(comm, &size);
        CORRT_MPI_COMM_RANK(comm, &rank);

        assert(!children && !parents && "Child/Parent lists allocated!?");
        err = setup_diss_graph(rank, size);
        if (MPI_SUCCESS != err) { return err; }

        int const i_corr_dist = read_env_int("CORRT_DIST"),
                  i_count_max = read_env_int("CORRT_COUNT_MAX");
        assert(i_corr_dist >= 0 && "Negative correction distance");
        assert(i_count_max >= 1 && "We need message size >=1 to accommodate the epoch");
        assert(corr_dist == 0xBEEFBABE && count_max == 0xBEEFBABE && "Params already initialised!?");

        corr_dist = (size_t) i_corr_dist;
        count_max = (size_t) i_count_max;

        // see description on declaration site!
        assert(req_diss_rcv == 0 &&
               req_corr_rcv == 0xBEEFBABE &&
               req_diss_snd == 0xBEEFBABE &&
               req_corr_snd == 0xBEEFBABE &&
               req_past_end == 0xBEEFBABE &&
               "Indices already initialised"
        );
        req_diss_rcv = 0;
        req_corr_rcv = req_diss_rcv + num_parent;
        req_diss_snd = req_corr_rcv + corr_dist;
        req_corr_snd = req_diss_snd + num_child;
        req_past_end = req_corr_snd + corr_dist;
    }

    assert(size > 0 && rank >=0 && rank < size && "MPI info not properly set.");
    assert((!num_child  || children) && "Child list not properly allocated.");
    assert((!num_parent || parents)  && "Parent list not properly allocated.");
    assert(corr_dist >= 0 && count_max >= 1 && "Params not initialised!?");
    assert(gossip_rounds <= 127 && "Too many Gossip rounds");


    if (1 == size) { return MPI_SUCCESS; } // that was simple :-)

    // Reject unexpected parameters ... it's only a prototype after all
    if ( (size_t)count > count_max ) {
        fprintf(stderr, "Message larger then specified maximum\n");
        return CORRT_ERR_NOT_IMPL;
    }


    /* On the first call to this function, i.e. the very first bcast, allocate
     * all dynamic memory and prepare the persistent receives which are normally
     * still present from the previous invocation.
     */
    if (epoch_global == 0) {
        err = allocate_buffers(module);
        if (MPI_SUCCESS != err) { return err; }

        // Non-root ranks expect to receive ...
        if (rank != root) {
//             assert(parent < (unsigned)rank && "Invalid parent in tree");
//             -> Not valid for Gossip

            // ... a diss message from each of their parents
            for (size_t par = 0; par < num_parent; ++par) {
                assert(parents[par] >= 0 && parents[par] < (unsigned)size && "Broken parent entry");
                err = CORRT_MPI_RECV_INIT(&buffers[(req_diss_rcv + par) * count_max],
                                     count_max,
                                     datatype,
                                     parents[par],
                                     MCA_COLL_BASE_TAG_BCAST,
                                     comm,
                                     &reqs[req_diss_rcv + par]);
                if (MPI_SUCCESS != err) { return err; }
            }


            // ... correction messages from their 'corr_dist' left neighbours
            for (size_t offset = 0; offset < corr_dist; ++offset) {
                ssize_t const sender = rank - (offset + 1); // receive from left

                // handle ranks close to start of the chain
                if (sender < 0) { break; }

                // we never get corr messages from our parents
                if (is_parent(sender)) { continue; }

                err = CORRT_MPI_RECV_INIT(&buffers[(req_corr_rcv + offset) * count_max],
                                     count_max,
                                     datatype,
                                     sender,
                                     MCA_COLL_BASE_TAG_BCAST,
                                     comm,
                                     &reqs[req_corr_rcv + offset]);
                if (MPI_SUCCESS != err) { return err; }
            }

            /* Activate all these receives ... manually
             *
             * We cannot use 'MPI_Startall' because we need to skip
             * 'MPI_REQUEST_NULL' requests as they are not allowed here:
             * "The argument, request, is a handle returned by one of the
             * previous five calls." -- MPI standard
             */
            for (size_t cc = req_diss_rcv; cc < req_diss_snd; ++cc) {
                if (reqs[cc] == MPI_REQUEST_NULL) { continue; }
                err = CORRT_MPI_START(&reqs[cc]);
                if (MPI_SUCCESS != err) { return err; }
            }
        }
    }

    assert(reqs && epoch_neigh && buff_fut && buffers && statuses && indices && (!num_child || children) && (!num_parent || parents) && "Memory not properly allocated.");
    assert( (rank != root || (MPI_REQUEST_NULL == reqs[req_diss_rcv]) ) && "Root with active diss recv");
    assert( (rank == root || !num_parent || (MPI_REQUEST_NULL != reqs[req_diss_rcv]) ) && "Non-root, non-orphaned node w/o (at least one) active diss recv");
    assert( (gossip_rounds < 0 || (unsigned)gossip_rounds == num_child) && "Inconsistent Gossip state");


    // Reset children we send to; normal case for trees, max for Gossip
    num_child_diss = num_child;


    /* Keep track which of our correction partners already sent a message to us
     *
     * This will be used for optimising the corr messages we send in the end.
     * We only need to remember the distance to sending node closest to us as
     * this will also cover the highest number of nodes we would need to send
     * correction to.
     *
     * The variable needs to start living here, because available "future"
     * messages might already affect it.
     */
    size_t corr_neigh = corr_dist; // init. value will allow for no optimisation

    if (rank == root) {
        // Add epoch to message (sacrificing part of user data)
        ((epoch_t*)buff)[0] = epoch_global;

        // Add remaining Gossip rounds to message (sacrificing more user data)
        if (gossip_rounds >= 0) {
            ((char*) &(((epoch_t*)buff)[1]))[0] = (char)gossip_rounds;
        }
    }
    // Non-root nodes need to receive a message first, recvs are already posted
    else {
        err = receive_initial_message(count,
                                      buff,
                                      &corr_neigh);
        if (MPI_SUCCESS != err) { return err; }
    } // end -- if (non-root rank)


    // At this point, 'buff' unconditionally contains the user's data. The
    // first 'char' has been overwritten with our (limited) epoch and for
    // Gossip the second 'char' contains the rounds left to go.


    /* Bulk-send tree messages to all our children */
    for (size_t offset = 0; offset < num_child; ++offset) {
        size_t const child = children[offset];
        assert(is_child(child) && "Inconsistent children");
        assert(req_diss_snd + offset < req_corr_snd && "Too many diss snds");

        // We don't do Gossip but a proper tree ... simply send asynchronously
        if (gossip_rounds < 0) {
            err = CORRT_MPI_ISEND(buff,
                             count,
                             datatype,
                             child,
                             MCA_COLL_BASE_TAG_BCAST,
                             comm,
                             &reqs[req_diss_snd + offset]);
        }

        else {
            /* How many rounds of Gossip are still left?
             *
             * We simplify Gossip a bit by not using real wallclock time
             * for the rounds. Instead, each message "consumes" exactly one
             * round, i.e., the receiver will send one message less then the
             * node that sent to him.
             *
             * For this to work, we tell the receiver how many rounds are still
             * left for him to do. Since this meand every child will get a
             * different round, we need to send synchronously (or allocate
             * multiple buffers which we don't)
             */
            char *const rounds_left = ((char*) &(((epoch_t*)buff)[1]));
            assert(*rounds_left >= 0 && "Message should not have been sent");

            // Gossip already finished
            if (*rounds_left == 0) {
                num_child_diss = offset; // we skip the remaining children
                break;
            }

            // Gossip still running
            else {
                // one round done by sending this
                --(*rounds_left);

                err = CORRT_MPI_SEND(buff,
                                     count,
                                     datatype,
                                     child,
                                     MCA_COLL_BASE_TAG_BCAST,
                                     comm);
            }
        }

        if (MPI_SUCCESS != err) { return err; }
    } //end -- send all dissemination messages

    assert(corr_neigh <= corr_dist && "Optimisation broken");
    assert((corr_neigh > 0 || !corr_dist) && "Optimisation broken");

    // If we don't do correction, we can just skip that part
    if (corr_dist) {
        err = do_correction(count,
                            datatype,
                            comm,
                            buff,
                            corr_neigh);
        if (MPI_SUCCESS != err) { return err; }
    }

    /* Wait for all (remaining) sends to finish since 'buff' may change after
     * we return to the user. Just ignore the return value as sends may fail.
     *
     * Again, it's ok to use all send reqs since irrelevant ones are set to
     * 'MPI_REQUEST_NULL' and thus will be ignored :-)
     */
    PMPI_Waitall(req_past_end - req_diss_snd,
                 &reqs[req_diss_snd],
                 MPI_STATUSES_IGNORE);

    ++epoch_global;
    assert(epoch_global < (epoch_t)~0 && "Epoch about to overflow");

    assert(MPI_SUCCESS == err);
    return err;
}
