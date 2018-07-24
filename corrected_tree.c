#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <mpi.h>
#include "util.h"
#include "corrected_binomial-tree.h"
#include "corrected_lame-tree.h"
#include "util.h"


/******************************************************************************
 * Corrected broadcast implementations                                        *
 ******************************************************************************/



/* Initialise all relevant tree parameters for this rank (second args line)
 *
 * Tree type is read from the environment.
 */
static bool
setup_tree(int const rank, int const comm_size,
           size_t *num_child, size_t *parent, size_t **children)
{
    assert(rank >= 0 && "Invalid rank");
    assert(comm_size > 0 && "Invalid size");
    assert(*num_child == 42 && *parent == 23 && *children == NULL && "Tree already initialised");

    char const * const tree_type = read_env_or_fail("TREE_TYPE");

    if (0 == strncmp(tree_type, "binomial", 8)) {
        return setup_tree_binomial(rank, comm_size, num_child, parent, children);
    }
    if (0 == strncmp(tree_type, "lame", 4)) {
        return setup_tree_lame(rank, comm_size, num_child, parent, children);
    }

    fprintf(stderr, "Unknown tree: '%s'\n", tree_type);
    PMPI_Abort(MPI_COMM_WORLD, -1);
    return false; // unreached
}

/* Test whether 'rank' is one of our children (i.e. is in the array) */
static bool
is_child(size_t const rank, size_t const num_child, size_t const * const children)
{
    for (size_t cc = 0; cc < num_child; ++cc) {
        assert(children[cc] > 0 && "Root is no child");
        if (children[cc] == rank) { return true; }
    }
    return false;
}


/* Check received message and (potentially) update our view of the sender's epoch
 *
 * returns 'true' if message is from current epoch,
           'false' for stale or future messages
 */
static bool
handle_receive(unsigned long long const        epoch_global,
               char               const        epoch_ltd,
               size_t             const        epoch_wrap,
               unsigned long long       *const epoch_neigh,
               char               const *const buffer)
{
    assert(epoch_global % epoch_wrap == (unsigned char) epoch_ltd && "Wrong limited epoch");

    /* Messages (including tree messages from our parent) might be
     * stale, i.e. still left over from previous epochs. We also need
     * to keep our view on other rank's epochs updated.
     */
    char const epoch_rcvd = buffer[0];
    long long const epoch_hyper_global = epoch_global / epoch_wrap,
                    epoch_hyper_neigh  = *epoch_neigh / epoch_wrap;

    assert(epoch_hyper_global >= epoch_hyper_neigh && "Neigbour is ahead");
    assert((epoch_hyper_global - epoch_hyper_neigh) <= 1 && "Neigbour is behind too far");
    assert((epoch_hyper_global != epoch_hyper_neigh ||
            epoch_ltd >= epoch_rcvd) && "Neigbour is ahead");

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
     * We are only guaranteed to get a message at least at the start of every
     * hyper epoch.
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


int
ompi_coll_base_bcast_intra_corrected(void *buff, int count,
                                     MPI_Datatype datatype,
                                     int root,
                                     MPI_Comm comm)
{
    static int const MCA_COLL_BASE_TAG_BCAST = 2342;

    int i_corr_dist = read_env_int("CORR_DIST"),
        i_count_max = read_env_int("CORR_COUNT_MAX");
    assert(i_corr_dist >= 0 && "Negative correction distance");
    assert(i_count_max >= 0 && "Negative maximum message size");

    size_t const corr_dist  = (size_t) i_corr_dist, // opportunistic correction distance
                 count_max  = (size_t) i_count_max, // maximum allowed message size
                 epoch_wrap = 128; // value at which the local epoch wraps

    /* Reject unexpected parameters ... it's only a prototype after all
     *
     * Also note that we're "stealing" the first 'char' in the user data to
     * trasmit the epoch. In a real implementation this would instead be part
     * of OMPI's internal metadata that establishes message order anyways.
     */
    if (root != 0 || datatype != MPI_CHAR || count < 1 || (size_t)count > count_max || comm != MPI_COMM_WORLD) {
        return MPI_ERR_INTERN;
    }


    /* =======================================================================
     * Following is info to be kept over all bcasts, thus they are 'static'
     *
     * Small value are stored directly while we allocate large buffers or those
     * whose size is not known at compile time as pointers to 'malloc'ed memory.
     */

    // Current epoch or broadcast round
    static unsigned long long epoch_global = 0;

    /* Request objects for sends and receives; see also 'req_*' below for
     * more info about the structure */
    static MPI_Request *reqs = NULL;

    /* Dedicated buffers for each of our receives; 'count_max' entries each
     *
     * [0] -> tree message from parent
     * [1 .. corr_dist] -> correction from 'corr_dist' left neighbours
     */
    static char *buffers = NULL;

    /* We keep the absolute epoch our correction partners are in.
     *
     * These values might not be accurate but rather a lower bound because
     * correction messages are optimised away.
     *
     * The indices are consistent with those in 'buffers'.
     */
    static unsigned long long *epoch_neigh = NULL;

    /* Does the respective rank's entry in 'buffers' contain valid data?
     *
     * This is the case if we already received a "future" broadcast from
     * a faster rank. (Only) Then the corresponding rcv operation in 'reqs'
     * will be inactive.
     */
    static bool *buff_fut = NULL;

    /* Arrays used for 'Testsome'/'Waitsome' */
    static MPI_Status *statuses = NULL;
    static int        *indices  = NULL;

    /* Tree structure */
    static size_t num_child = 42,   // number of child ranks
                  parent    = 23,   // rank of our parent
                  *children = NULL; // array of child ranks

    /* ==== end of static info ============================================== */

    int size, rank;
    PMPI_Comm_size(comm, &size);
    PMPI_Comm_rank(comm, &rank);

    if (1 == size) { return MPI_SUCCESS; }



    /* On the first call to this function, i.e. the very first bcast, prepare
     * basic information for the tree to be used.
     */
    if (epoch_global == 0) {
        assert(!children && "Child list allocated!?");

        if (!setup_tree(rank, size, &num_child, &parent, &children)) {
            return MPI_ERR_NO_MEM;
        }
    }
    assert((!num_child || children) && "Child list not properly allocated.");

    /* Principle structure of the 'reqs' array of request objects:
     *
     * => 'req_tree_rcv' refers to the next (start) index
     * [0] -> recv from parent
     *
     * => 'req_corr_rcv' refers to the next (start) index
     * [1 .. corr_dist] -> recv correction from right neighbours
     *
     * => 'req_tree_snd' refers to the next (start) index
     * [corr_dist + 1 .. corr_dist + num_child] -> send to children in tree
     *
     * => 'req_corr_snd' refers to the next (start) index
     * [corr_dist + num_child + 1 ..] -> send correction to left neighbours
     *
     *
     * For receives, which are persistant requests, the same indices are also
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
    size_t const req_tree_rcv = 0,
                 req_corr_rcv = req_tree_rcv + 1,
                 req_tree_snd = req_corr_rcv + corr_dist,
                 req_corr_snd = req_tree_snd + num_child,
                 req_past_end = req_corr_snd + corr_dist;

    int err = MPI_SUCCESS;


    /* On the first call to this function, i.e. the very first bcast, allocate
     * all dynamic memory and prepare the receives which are normally still
     * present from the previous invocation.
     */
    if (epoch_global == 0) {
        assert(!reqs && !epoch_neigh && !buff_fut && !buffers && !statuses && !indices && "Memory already allocated!?");

        // TODO: If any ressource allocation fails, we just leak previously
        //       allocated ressources. Same for later communication problems ...
        //       prototype, you know ;-)

        // Dedicated buffers for each of the receives; 'count_max' entries each
        buffers = malloc( (1 + corr_dist) * count_max * sizeof(char) );
        if (!buffers) { return MPI_ERR_NO_MEM; }

        /* For our parent and each of our "correction neighbours" we store the
         * (absolute) epoch in which we expect to get the next message from
         * them; initally that's '0'.
         * If there has already been a "future message", we remember the epoch
         * of that message, which is held in the neighbour's rcv buffer.
         */
        epoch_neigh = calloc(1 + corr_dist, sizeof(long long));
        if (!epoch_neigh) { return MPI_ERR_NO_MEM; }

        // Flags to keep track of already-received future messages
        buff_fut = calloc(1 + corr_dist, sizeof(bool));
        if (!buff_fut) { return MPI_ERR_NO_MEM; }

        /* Request objects for all sends + receives (init to 'MPI_REQUEST_NULL'!) */
        reqs = malloc( (1 + num_child + 2 * corr_dist) * sizeof(MPI_Request) );
        if (!reqs) { return MPI_ERR_NO_MEM; }

        for (int cc=0; cc<1 + num_child + 2 * corr_dist; ++cc) {
            reqs[cc] = MPI_REQUEST_NULL;
        }

        /* Arrays for 'Waitsome' */
        statuses  = malloc( (1 + num_child + 2 * corr_dist) * sizeof(MPI_Status) );
        if (!statuses) { return MPI_ERR_NO_MEM; }
        indices = malloc( (1 + num_child + 2 * corr_dist) * sizeof(int) );
        if (!indices) { return MPI_ERR_NO_MEM; }

        assert(reqs && epoch_neigh && buff_fut && buffers && statuses && indices && "Memory not properly allocated.");

        // Non-root ranks expect to receive ...
        if (rank != root) {
            assert(parent < (unsigned)rank && "Invalid parent");

            // ... a tree message from their parent
            err = PMPI_Recv_init(&buffers[req_tree_rcv * count_max],
                                 count_max,
                                 datatype,
                                 parent,
                                 MCA_COLL_BASE_TAG_BCAST,
                                 comm,
                                 &reqs[req_tree_rcv]);
            if (MPI_SUCCESS != err) { return err; }


            // ... correction messages from their 'corr_dist' left neighbours
            for (size_t offset = 0; offset < corr_dist; ++offset) {
                ssize_t const sender = rank - (offset + 1); // receive from left

                // handle ranks close to start of the chain
                if (sender < 0) { break; }

                // we never get corr messages from our parent
                if ((size_t)sender == parent) { continue; }

                err = PMPI_Recv_init(&buffers[(req_corr_rcv + offset) * count_max],
                                     count_max,
                                     datatype,
                                     sender,
                                     MCA_COLL_BASE_TAG_BCAST,
                                     comm,
                                     &reqs[req_corr_rcv + offset]);
                if (MPI_SUCCESS != err) { return err; }
            }

            /* Activate all receives ... manually
             *
             * We cannot use 'MPI_Startall' because we need to skip
             * 'MPI_REQUEST_NULL' requests since they are not allowed here:
             * "The argument, request, is a handle returned by one of the
             * previous five calls." -- MPI standard
             */
            for (size_t cc = req_tree_rcv; cc < req_tree_snd; ++cc) {
                if (reqs[cc] == MPI_REQUEST_NULL) { continue; }
                err = PMPI_Start(&reqs[cc]);
                if (MPI_SUCCESS != err) { return err; }
            }
//             err = PMPI_Startall(req_tree_snd - req_tree_rcv, &reqs[req_tree_rcv]);
//             if (MPI_SUCCESS != err) { return err; }
        }
    }

    assert(reqs && epoch_neigh && buff_fut && buffers && statuses && indices && (!num_child || children) && "Memory not properly allocated.");
    assert( (rank != root || (MPI_REQUEST_NULL == reqs[req_tree_rcv]) ) && "Root with active tree recv");
    assert( (rank == root || (MPI_REQUEST_NULL != reqs[req_tree_rcv]) ) && "Non-root w/o active tree recv");

    // Current epoch, limited to the size of a 'char' or rather 'epoch_wrap'
    // This is what we send around with the user's payload
    char const epoch_ltd = epoch_global % epoch_wrap;

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
        // Add limited-size epoch to message (sacrificing part of user data)
        ((char*)buff)[0] = epoch_ltd;
    }
    // Non-root nodes need to receive a message first, recvs are already posted
    else {
        size_t idx;        // index of (last) successful rcv request
        bool done = false; // got a message (for current epoch) yet?

        /* We might already have received a/some "future" corr message(s) for
         * the current epoch...
         *
         * Note that we need to find *all* "future" corr mesages for this
         * epoch and reenable the respective receives.
         */
        for (idx = 0; idx <= corr_dist; ++idx) {
            // Only exact epoch matches are relevant here
            if ( ! (buff_fut[idx] && epoch_neigh[idx] == epoch_global) ) {
                /* Should at most be from the start of the next hyperepoch:
                 * Everybody sends at that point and we stop receiving once
                 * we got any message from the future.
                 */
                assert(epoch_neigh[idx] < ((epoch_global / epoch_wrap) + 1) * epoch_wrap || "Message from far future");

                continue;
            }

            /* We actually got the right message, store it in the user's buffer.
             * Doing this once is enough however.
             *
             * Note: This *must* be done before we reactivate the rcv! Races...
             */
            if (!done) {
                memcpy(buff, &buffers[idx * count_max], count);
                done = true;
            }
            // use the opportunity of two future messages to check for consistency
            else {
                assert(buffers[idx * count_max] >= 0 && "Negative epoch rcvd");
                assert( (epoch_neigh[idx] % epoch_wrap) == (unsigned char)buffers[idx * count_max] && "Future message with wrong epoch");
                assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
            }

            // This will have an effect on our own correction
            // (unless the message is from our parent)
            corr_neigh = (idx && idx < corr_neigh) ? idx : corr_neigh;

            // Reactivate the receive request, now that we caught up
            err = PMPI_Start(&reqs[req_tree_rcv + idx]);
            if (MPI_SUCCESS != err) { return err; }
        } // end -- handle stored future messages for this epoch

        /* If we don't have the message for this epoch yet, wait for somebody to
         * send it to us: drop stale messages, reactivate recvs, store the
         * payload in the user's buffer once rcv'd and properly handle "future"
         * corr messages
         *
         * Note that we will not see future tree messages here since we exit the
         * loop once we have the current message and our parent does not skip
         * epochs.
         */
        while (!done) {
            int completed = MPI_UNDEFINED; // better be safe and initialise it

            /* It's fine to use all recv reqs since irrelevant ones are set to
             * 'MPI_REQUEST_NULL' or inactive and thus will be ignored :-)
             */
            err = PMPI_Waitsome(req_tree_snd - req_tree_rcv, // incount
                                &reqs[req_tree_rcv], // array of requests
                                &completed,          // outcount
                                indices,             // array of indices
                                statuses);           // array of statuses
            if (MPI_SUCCESS != err) { return err; }

            assert(completed != MPI_UNDEFINED && "No active requests!?");

            /* Handle *all* completed requests */
            for (int cc = 0; cc < completed; ++cc) {
                size_t const               idx    = indices[cc];
                MPI_Status const status = statuses[cc];

                assert(status.MPI_SOURCE >= 0 && "Invalid sender rank");
                assert(rank - status.MPI_SOURCE > 0 && "Message (parent/correction) from right");
                assert(idx == ( (unsigned)status.MPI_SOURCE == parent ? 0U : rank - (unsigned)status.MPI_SOURCE ) && "Unexpected index for sender");

                /* Note that 'idx' "incidentally" corresponds to the sender rank's
                 * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
                 */
                bool matches = handle_receive(epoch_global,
                                              epoch_ltd,
                                              epoch_wrap,
                                              &epoch_neigh[idx],
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
                        assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
                    }

                    // Note: The previous *must* be done before we reactivate the send! Races...

                    // This will have an effect on our own correction
                    // (unless the message came from our parent)
                    corr_neigh = (idx && idx < corr_neigh) ? idx : corr_neigh;
                }
                // Future message
                // Note: Nothing special to do for stale messages
                else if (epoch_neigh[idx] > epoch_global) {
                    assert(idx != 0 && "Future tree message from parent");

                    buff_fut[idx] = true;
                    continue; // do *not* reactivate the rcv or increase the epoch
                }

                // Reactivate the completed recv
                err = PMPI_Start(&reqs[req_tree_rcv + idx]);
                if (MPI_SUCCESS != err) { return err; }

                // Next time, we expect the next epoch from him, of course
                ++epoch_neigh[idx];
            } // end -- handle completed requests
        } // end -- wait for a message for this epoch
    } // end -- if (non-root rank)


    // At this point, 'buff' unconditionally contains the user's data. The
    // first 'char' has been overwritten with our (limited) epoch.


    /* Bulk-send tree messages to all our children */
    for (size_t offset = 0; offset < num_child; ++offset) {
        size_t const child = children[offset];
        assert(is_child(child, num_child, children) && "Inconsistent children");

        assert(req_tree_snd + offset < req_corr_snd && "Too many tree snds");
        err = PMPI_Isend(buff,
                         count,
                         datatype,
                         child,
                         MCA_COLL_BASE_TAG_BCAST,
                         comm,
                         &reqs[req_tree_snd + offset]);
        if (MPI_SUCCESS != err) { return err; }
    }

    /* Send correction messages to our 'corr_dist' right neighbours
     *
     * Do not send to our children or to root (implicitly since it's either to
     * our "left" or "beyond the chain" as we don't close the ring).
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
     */

    assert(corr_neigh <= corr_dist && "Optimisation broken");
    assert((corr_neigh > 0 || !corr_dist) && "Optimisation broken");

    bool first = true; // first round in the correction phase?
    size_t offset = 0;

    // If we don't do correction ('corr_dist' == 0), we can just skip that part
    // Note that this is a constant! We break from the loop to leave it...
    while (corr_dist) {
        bool done = first; // sent of current corr message done? (no message is being sent in the first round!)

        do {
            int completed = MPI_UNDEFINED; // better be safe and initialise it

            /* It's fine to use all reqs since irrelevant ones are set to
             * 'MPI_REQUEST_NULL' or inactive and thus will be ignored :-)
             */
            if (first) {
                // do not wait yet, just check if request are already done
                err = PMPI_Testsome(req_past_end - req_tree_rcv, // incount
                                    &reqs[req_tree_rcv], // array of requests
                                    &completed,          // outcount
                                    indices,             // array of indices
                                    statuses);           // array of statuses
                first = false;
            }
            else {
                err = PMPI_Waitsome(req_past_end - req_tree_rcv, // incount
                                    &reqs[req_tree_rcv], // array of requests
                                    &completed,          // outcount
                                    indices,             // array of indices
                                    statuses);           // array of statuses
            }
            assert(completed != MPI_UNDEFINED && "No active requests!?");

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
                size_t     const idx    = indices[cc];
                MPI_Status const status = statuses[cc];

                // send finished
                if (idx >= req_tree_snd) {
                    // Was it a (i.e. our current) corr message?
                    // Note: We don't care much about tree sends here...
                    if (idx >= req_corr_snd) {
                        assert(idx == req_corr_snd + offset - 1 && "Unexpected send ");
                        done = true;
                    }

                    // That's all, just handle the next completion
                    continue;
                }
                // receive finished
                else {
                    assert( (MPI_SUCCESS == err || MPI_SUCCESS == status.MPI_ERROR) && "Failed recv");
                    assert(rank > status.MPI_SOURCE && "Correction from right neighbour (or weird/small parent rank)");
                    assert((int)idx == ((unsigned)status.MPI_SOURCE == parent ? 0 : rank - status.MPI_SOURCE) && "Unexpected index for sender");

                    /* Note that 'idx' "incidentally" corresponds to the sender rank's
                     * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
                     */
                    bool matches = handle_receive(epoch_global,
                                                  epoch_ltd,
                                                  epoch_wrap,
                                                  &epoch_neigh[idx],
                                                  &buffers[idx * count_max]);

                    assert(matches == (epoch_neigh[idx] == epoch_global) && "Function broken!?");

                    // It's the right epoch, but we already got the message before...
                    if (matches) {
                        // ... use the opportunity for a consistency check
                        // Note: This *must* be done before we reactivate the send! Races...
                        assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");

                        // keep track of the closest sender
                        // (unless the message came from our parent)
                        // Note: 'idx' "conincidentely" is our distance from the sender
                        corr_neigh = (idx && idx < corr_neigh) ? idx : corr_neigh;
                    }
                    // Future message
                    // Note: Nothing special to do for stale messages
                    else if (epoch_neigh[idx] > epoch_global) {
                        buff_fut[idx] = true;
                        continue; // do *not* reactivate the rcv or increase epoch, handle next completion
                    }
                } // end -- recv

                // Reactivate completed recv
                err = PMPI_Start(&reqs[req_tree_rcv + idx]);
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
         * Skip this optimisation at the start of new hyper epochs
         * ('epoch_ltd' == 0). There we *always* send corrections to maintain
         * the sync between different ranks.
         *
         * Note: 'offset' indicates the distance to the rank we just sent to.
         */
        if (epoch_ltd) {
            assert(corr_neigh <= corr_dist && "Optimisation broken");
            size_t const handled_by_sender = corr_dist - corr_neigh;
            offset = (offset > handled_by_sender) ? offset : handled_by_sender;
        }

        int receiver;

        // Find the next receiver, but do not send to children
        do {
            ++offset;
            receiver = rank + offset; // send to right
        } while (is_child(receiver, num_child, children));

//         if (send_over_root) {
//             receiver = (receiver + size) % size;
//             if (receiver == root) {continue;}
//         }

        // Handle end of the chain (1st part) or end of correction (2nd part)
        if (receiver >= size || offset > corr_dist) { break; }

        err = PMPI_Isend(buff,
                         count,
                         datatype,
                         receiver,
                         MCA_COLL_BASE_TAG_BCAST,
                         comm,
                         &reqs[req_corr_snd + offset - 1]);
        if (MPI_SUCCESS != err) { return err; }

    } // end -- cover 'corr_dist' neighbours with 'offset'


    /* Wait for all (remaining) sends to finish since 'buff' may change after
     * we return to the user. Just ignore the return value as sends may fail.
     *
     * Again, it's ok to use all send reqs since irrelevant ones are set to
     * 'MPI_REQUEST_NULL' and thus will be ignored :-)
     */
    PMPI_Waitall(req_past_end - req_tree_snd,
                 &reqs[req_tree_snd],
                 MPI_STATUSES_IGNORE);

    ++epoch_global;

    assert(MPI_SUCCESS == err);
    return err;
}
