#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>

#include <mpi.h>

/******************************************************************************
 * Corrected trees broadcast implementation                                   *
 ******************************************************************************/

/* Find MSB in a given number
 *
 * Approach taken from simulator
 */
static int
ffmk_get_msb(int num)
{
    int msb = 0;

    // Find most significant bit
    while (num) {
        num /= 2;
        ++msb;
    }
    return msb;
}

/* Check received message and (potentially) update our view of the sender's epoch
 *
 * returns 'true' if message is from current epoch,
           'false' for stale or future messages
 */
static bool
ffmk_handle_receive(long long const        epoch_global,
                    char      const        epoch_ltd,
                    int       const        epoch_wrap,
                    long long       *const epoch_neigh,
                    char      const *const buffer)
{
    assert(epoch_global % epoch_wrap == epoch_ltd && "Wrong limited epoch");

    /* Messages (including tree messages from our parent) might be
     * stale, i.e. still left over from previous epochs. We also need
     * to keep our view on other rank's epochs updated.
     */
    char const epoch_rcvd = buffer[0];
    long long const epoch_hyper_global = epoch_global / epoch_wrap,
                    epoch_hyper_neigh  = *epoch_neigh / epoch_wrap;

    assert(epoch_hyper_global >= epoch_hyper_neigh && "Neigbour is ahead");
    assert((epoch_hyper_global - epoch_hyper_neigh) <= 1 && "Neigbour is behind too far");
    assert(epoch_hyper_global != epoch_hyper_neigh ||
           epoch_ltd >= epoch_rcvd && "Neigbour is ahead");

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
    static int corr_dist = 11,    // opportunistic correction distance
               count_max = 512,   // maximum allowed message size
               epoch_wrap = 128; // value at which the local epoch wraps
/* TODO: be more flexble with the corr distance (MCA or ENV?) and count_max(??) */

    /* Reject unexpected parameters ... it's only a prototype after all
     *
     * Also note that we're "stealing" the first 'char' in the user data to
     * trasmit the epoch. In a real implementation this would instead be part
     * of OMPI's internal metadata that establishes message order anyways.
     */
    if (root != 0 || datatype != MPI_CHAR || count < 1 || count > count_max || comm != MPI_COMM_WORLD) {
        return MPI_ERR_INTERN;
    }


    /* =======================================================================
     * Following is info to be kept over all bcasts, thus they are 'static'
     *
     * Small value are stored directly while we allocate large buffers or those
     * whose size is not known at compile time as pointers to 'malloc'ed memory.
     */

    // Current epoch or broadcast round
    static long long epoch_global = 0;

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
    static long long *epoch_neigh = NULL;

    /* Does the respective rank's entry in 'buffers' contain valid data?
     *
     * This is the case if we already received a "future" broadcast from
     * a faster rank. (Only) Then the corresponding rcv operation in 'reqs'
     * will be inactive.
     */
    static bool *buff_fut = NULL;

    /* Arrays used for 'Waitsome' */
    static MPI_Status *statuses = NULL;
    static int        *indices  = NULL;

    /* ==== end of static info ============================================== */

    int size, rank;
    PMPI_Comm_size(comm, &size);
    PMPI_Comm_rank(comm, &rank);

    if (1 == size) { return MPI_SUCCESS; }


    /* Determine number of our children */
    int const len_rank  = ffmk_get_msb(rank),     // length/bits of rank ID
              len_tree  = ffmk_get_msb(size - 1), // max length/bits of rank IDs
              num_child = len_tree - len_rank,    // remaining bits = children
              parent    = rank & ~(1 << (len_rank-1)); // drop MSB of rank ID to find our parent

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
            for (int offset = 0; offset < corr_dist; ++offset) {
                int const sender = rank - (offset + 1); // receive from left

                // we never get corr messages from our parent
                if (sender == parent) { continue; }

                // handle ranks close to start of the chain
                if (sender < 0) { break; }

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
             * previous five calls."
             */
            for (int cc = req_tree_rcv; cc < req_tree_snd; ++cc) {
                if (reqs[cc] == MPI_REQUEST_NULL) { continue; }
                PMPI_Start(&reqs[cc]);
            }
//             err = PMPI_Startall(req_tree_snd - req_tree_rcv, &reqs[req_tree_rcv]);
//             if (MPI_SUCCESS != err) { return err; }
        }
    }

    assert(reqs && epoch_neigh && buff_fut && buffers && statuses && indices && "Memory not properly allocated.");
    assert( (rank != root || (MPI_REQUEST_NULL == reqs[req_tree_rcv]) ) && "Root with active tree recv");
    assert( (rank == root || (MPI_REQUEST_NULL != reqs[req_tree_rcv]) ) && "Non-root w/o active tree recv");

    // Current epoch, limited to the size of a 'char' or rather 'epoch_wrap'
    // This is what we send around with the user's payload
    char const epoch_ltd = epoch_global % epoch_wrap;

    if (rank == root) {
        // Add limited-size epoch to message (sacrificing part of user data)
        ((char*)buff)[0] = epoch_ltd;
    }
    // Non-root nodes need to receive a message first, recvs are already posted
    else {
        int idx;           // index of (last) successful rcv request
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
            else {
                assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
            }

            // Reactivate the receive request, now that we caught up
            err = PMPI_Start(&reqs[req_tree_rcv + idx]);
            if (MPI_SUCCESS != err) { return err; }
        }

        /* Wait for somebody to send to us: drop stale messages, reactivate recvs,
         * store user data once rcv'd and properly handle "future" corr messages
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
            assert(completed != MPI_UNDEFINED && "No active requests!?");

            // Do *not* check 'status.MPI_ERROR' as it is not set - see section 3.2.5 of MPI 3.0
            if (MPI_SUCCESS != err) { return err; }

            /* Handle *all* completed requests */
            for (int cc = 0; cc < completed; ++cc) {
                int        const idx    = indices[cc];
                MPI_Status const status = statuses[cc];

                assert(idx == (status.MPI_SOURCE == parent ? 0 : rank - status.MPI_SOURCE) && "Unexpected index");

                /* Note that 'idx' "incidentally" corresponds to the sender rank's
                 * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
                 */
                bool matches = ffmk_handle_receive(epoch_global,
                                                   epoch_ltd,
                                                   epoch_wrap,
                                                   &epoch_neigh[idx],
                                                   &buffers[idx * count_max]);

if (matches != (epoch_neigh[idx] == epoch_global)) {
    fprintf(stderr, "matches = %d, epoch_neigh[idx] = %lld, epoch_global= %lld, epoch_rcvd = %d\n" , matches, epoch_neigh[idx], epoch_global, buffers[idx * count_max]);
}
                assert(matches == (epoch_neigh[idx] == epoch_global) && "Function broken!?");


                // We actually got the right message, so...
                if (matches) {
                    // ... store it in the user's if we didn't have a match before
                    if (!done) {
                        memcpy(buff, &buffers[idx * count_max], count);
                        done = true;
                    }
                    // ... or use the opportunity for a consistency check
                    else {
                        assert(0 == memcmp(buff, &buffers[idx * count_max], count * sizeof(char)) && "Differing payloads for same epoch");
                    }

                    // Note: This *must* be done before we reactivate the send! Races...
                }
                // Future message
                // Note: Nothing special to do for stale messages
                else if (!matches && epoch_neigh[idx] > epoch_global) {
                    assert(idx != 0 && "Future tree message from parent");

                    buff_fut[idx] = true;
                    continue; // do *not* reactivate the rcv
                }

                // Reactivate completed recv
                err = PMPI_Start(&reqs[req_tree_rcv + idx]);
                if (MPI_SUCCESS != err) { return err; }

                // Next time, we expect the next epoch from him, of course
                ++epoch_neigh[idx];
            } // end -- handle completed requests
        } // end -- while (!done)
    } // end -- if (non-root rank)


    /* Bulk-send tree messages to all our children
     *
     * Child ranks are found by setting each unused bit above our MSB in turn
     */
    for (int offset = 0; offset < num_child; ++offset) {
        assert(len_rank + offset + 1 <= len_tree && "Too many children");

        int const child = rank + (1 << (len_rank + offset));

        // handle non-full tree
        if (child >= size) { break; }

        // At this point, 'buff' unconditionally contains the user's data. The
        // first 'char' has been overwritten with our (limited) epoch.
        assert(req_tree_snd + offset < req_corr_snd && "Too many tree snds");
        err = PMPI_Isend(buff,
                         count,
                         MPI_CHAR,
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
     * As a (possible) optimisation, we send correction one by one. Also, we
     * ommit corr messages to those ranks that we know will be covered be others
     * (that already sent to us).
     *
     * Example: (R-3) (R-2) (R-1)  (R)  (R+1) (R+2) (R+3)
     *
     * If we received from (R-1) and 'corr_dist' == 3, we know (R-1) will also
     * send corr to (R+1) and (R+2).
     *
     * Note that we do *not* try to receive from our parent. First, we don't
     * want "future" messages from him. Second, we'd need to special-case as
     * those are not corr messages.
     */
    for (int offset = 1; offset <= corr_dist; ++offset) {
        int const receiver = rank + offset; // send to right

//         if (send_over_root) {
//             receiver = (receiver + size) % size;
//             if (receiver == root) {continue;}
//         }

        if (receiver >= size) { break; }

        // Do not send to children
        for (int cc = 0; cc < num_child; ++cc) {
            assert(len_rank + cc + 1 <= len_tree && "Too many children");

            int const child = rank + (1 << (len_rank + cc));

            if (child == receiver) { goto end_corr_loop; }
        }

        err = PMPI_Isend(buff,
                         count,
                         datatype,
                         receiver,
                         MCA_COLL_BASE_TAG_BCAST,
                         comm,
                         &reqs[req_corr_snd + offset - 1]);
        if (MPI_SUCCESS != err) { return err; }

        bool done = false; // was the current corr message sent?

        // (highest) offset of any rank that sent us correction in the
        // current round
        int sender_offset = 0;

        while (!done) {
            int completed = MPI_UNDEFINED; // better be safe and initialise it

            /* It's fine to use all reqs since irrelevant ones are set to
             * 'MPI_REQUEST_NULL' or inactive and thus will be ignored :-)
             */
            err = PMPI_Waitsome(req_past_end - req_corr_rcv, // incount
                                &reqs[req_corr_rcv], // array of requests
                                &completed,          // outcount
                                indices,             // array of indices
                                statuses);           // array of statuses
            assert(completed != MPI_UNDEFINED && "No active requests!?");

            if (MPI_SUCCESS != err && MPI_ERR_IN_STATUS != err) { return err; };

            /* Handle *all* completed requests */
            for (int cc = 0; cc < completed; ++cc) {
                /* Beware an OBOE with index 'idx'
                 *
                 * We only waited for requests starting from 'req_corr_rcv'!
                 * Compensate for that now so the right buffer + req is used.
                 */
                assert(indices[cc] >= 0 && "Waitsome broken!?");
                size_t     const idx    = indices[cc] + req_corr_rcv;
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
                // (corr) receive finished
                else {
                    assert(MPI_SUCCESS == status.MPI_ERROR && "Failed recv (TODO really set properly in status!?)");
                    assert(rank > status.MPI_SOURCE && "Correction from right neighbour");
                    assert(idx == (size_t)(rank - status.MPI_SOURCE) && "Unexpected index");

                    /* Note that 'idx' "incidentally" corresponds to the sender rank's
                     * entries not only in 'reqs' but also 'buffers' and 'epoch_neigh'.
                     */
                    bool matches = ffmk_handle_receive(epoch_global,
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

                        // keep track of the highest sender offset
                        // Note: 'idx' "conincidentely" is our distance from the sender
                        sender_offset = (sender_offset > idx ? sender_offset : idx);
                    }
                    // Future message
                    // Note: Nothing special to do for stale messages
                    else if (!matches && epoch_neigh[idx] > epoch_global) {
                        buff_fut[idx] = true;
                        continue; // do *not* reactivate the rcv, handle next completion
                    }
                } // end -- corr recv

                // Reactivate completed recv
                err = PMPI_Start(&reqs[req_tree_rcv + idx]);
                if (MPI_SUCCESS != err) { return err; }

                // Next time, we expect the next epoch from him, of course
                ++epoch_neigh[idx];
            } // end -- handle completed requests
        } // end -- while (current corr message not sent)

        /* Our current corr message has been sent now. Before considering the
         * next corr message, update 'offset' so ranks covered by anybody who
         * sent to us is not bothered by us as well.
         *
         * Skip this optimisation at the start of new hyper epochs. There we
         * *always* send corrections to maintain the sync between different
         * ranks.
         *
         * Note: 'offset' indicates the rank we just sent to.
         *       It's increased in the loop header before the
         *       next send or test for corr completion.
         */
        if (epoch_ltd) {
            int const handled_by_sender = corr_dist - sender_offset;
            offset = (offset > handled_by_sender ? offset : handled_by_sender);
        }

end_corr_loop:
        ;
    }

    /* Wait for all (remaining) sends to finish, just ignore the return value
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
