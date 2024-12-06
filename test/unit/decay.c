#include "test/jemalloc_test.h"

#include "jemalloc/internal/decay.h"

TEST_BEGIN(test_decay_init) {
	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	ssize_t decay_ms = 1000;
	assert_true(decay_ms_valid(decay_ms), "");

	expect_false(decay_init(&decay, &curtime, decay_ms),
	    "Failed to initialize decay");
	expect_zd_eq(decay_ms_read(&decay), decay_ms,
	    "Decay_ms was initialized incorrectly");
	expect_u64_ne(decay_epoch_duration_ns(&decay), 0,
	    "Epoch duration was initialized incorrectly");
}
TEST_END

<<<<<<< HEAD
TEST_BEGIN(test_decay_ms_valid) {
	expect_false(decay_ms_valid(-7),
	    "Misclassified negative decay as valid");
	expect_true(decay_ms_valid(-1),
	    "Misclassified -1 (never decay) as invalid decay");
	expect_true(decay_ms_valid(8943),
	    "Misclassified valid decay");
	if (SSIZE_MAX > NSTIME_SEC_MAX) {
		expect_false(
		    decay_ms_valid((ssize_t)(NSTIME_SEC_MAX * KQU(1000) + 39)),
		    "Misclassified too large decay");
=======
static bool
nstime_monotonic_mock(void) {
	return monotonic_mock;
}

static bool
nstime_update_mock(nstime_t *time) {
	nupdates_mock++;
	if (monotonic_mock) {
		nstime_copy(time, &time_mock);
	}
	return !monotonic_mock;
}

static unsigned
do_arena_create(ssize_t dirty_decay_ms, ssize_t muzzy_decay_ms) {
	unsigned arena_ind;
	size_t sz = sizeof(unsigned);
	assert_d_eq(mallctl("arenas.create", (void *)&arena_ind, &sz, NULL, 0),
	    0, "Unexpected mallctl() failure");
	size_t mib[3];
	size_t miblen = sizeof(mib)/sizeof(size_t);

	assert_d_eq(mallctlnametomib("arena.0.dirty_decay_ms", mib, &miblen),
	    0, "Unexpected mallctlnametomib() failure");
	mib[1] = (size_t)arena_ind;
	assert_d_eq(mallctlbymib(mib, miblen, NULL, NULL,
	    (void *)&dirty_decay_ms, sizeof(dirty_decay_ms)), 0,
	    "Unexpected mallctlbymib() failure");

	assert_d_eq(mallctlnametomib("arena.0.muzzy_decay_ms", mib, &miblen),
	    0, "Unexpected mallctlnametomib() failure");
	mib[1] = (size_t)arena_ind;
	assert_d_eq(mallctlbymib(mib, miblen, NULL, NULL,
	    (void *)&muzzy_decay_ms, sizeof(muzzy_decay_ms)), 0,
	    "Unexpected mallctlbymib() failure");

	return arena_ind;
}

static void
do_arena_destroy(unsigned arena_ind) {
	size_t mib[3];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib("arena.0.destroy", mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[1] = (size_t)arena_ind;
	assert_d_eq(mallctlbymib(mib, miblen, NULL, NULL, NULL, 0), 0,
	    "Unexpected mallctlbymib() failure");
}

void
do_epoch(void) {
	uint64_t epoch = 1;
	assert_d_eq(mallctl("epoch", NULL, NULL, (void *)&epoch, sizeof(epoch)),
	    0, "Unexpected mallctl() failure");
}

void
do_purge(unsigned arena_ind) {
	size_t mib[3];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib("arena.0.purge", mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[1] = (size_t)arena_ind;
	assert_d_eq(mallctlbymib(mib, miblen, NULL, NULL, NULL, 0), 0,
	    "Unexpected mallctlbymib() failure");
}

void
do_decay(unsigned arena_ind) {
	size_t mib[3];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib("arena.0.decay", mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[1] = (size_t)arena_ind;
	assert_d_eq(mallctlbymib(mib, miblen, NULL, NULL, NULL, 0), 0,
	    "Unexpected mallctlbymib() failure");
}

static uint64_t
get_arena_npurge_impl(const char *mibname, unsigned arena_ind) {
	size_t mib[4];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib(mibname, mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[2] = (size_t)arena_ind;
	uint64_t npurge = 0;
	size_t sz = sizeof(npurge);
	assert_d_eq(mallctlbymib(mib, miblen, (void *)&npurge, &sz, NULL, 0),
	    config_stats ? 0 : ENOENT, "Unexpected mallctlbymib() failure");
	return npurge;
}

static uint64_t
get_arena_dirty_npurge(unsigned arena_ind) {
	do_epoch();
	return get_arena_npurge_impl("stats.arenas.0.dirty_npurge", arena_ind);
}

static uint64_t
get_arena_muzzy_npurge(unsigned arena_ind) {
	do_epoch();
	return get_arena_npurge_impl("stats.arenas.0.muzzy_npurge", arena_ind);
}

static uint64_t
get_arena_npurge(unsigned arena_ind) {
	do_epoch();
	return get_arena_npurge_impl("stats.arenas.0.dirty_npurge", arena_ind) +
	    get_arena_npurge_impl("stats.arenas.0.muzzy_npurge", arena_ind);
}

static size_t
get_arena_pdirty(unsigned arena_ind) {
	do_epoch();
	size_t mib[4];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib("stats.arenas.0.pdirty", mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[2] = (size_t)arena_ind;
	size_t pdirty;
	size_t sz = sizeof(pdirty);
	assert_d_eq(mallctlbymib(mib, miblen, (void *)&pdirty, &sz, NULL, 0), 0,
	    "Unexpected mallctlbymib() failure");
	return pdirty;
}

static size_t
get_arena_pmuzzy(unsigned arena_ind) {
	do_epoch();
	size_t mib[4];
	size_t miblen = sizeof(mib)/sizeof(size_t);
	assert_d_eq(mallctlnametomib("stats.arenas.0.pmuzzy", mib, &miblen), 0,
	    "Unexpected mallctlnametomib() failure");
	mib[2] = (size_t)arena_ind;
	size_t pmuzzy;
	size_t sz = sizeof(pmuzzy);
	assert_d_eq(mallctlbymib(mib, miblen, (void *)&pmuzzy, &sz, NULL, 0), 0,
	    "Unexpected mallctlbymib() failure");
	return pmuzzy;
}

static void *
do_mallocx(size_t size, int flags) {
	void *p = mallocx(size, flags);
	assert_ptr_not_null(p, "Unexpected mallocx() failure");
	return p;
}

static void
generate_dirty(unsigned arena_ind, size_t size) {
	int flags = MALLOCX_ARENA(arena_ind) | MALLOCX_TCACHE_NONE;
	void *p = do_mallocx(size, flags);
	dallocx(p, flags);
}

TEST_BEGIN(test_decay_ticks) {
	test_skip_if(check_background_thread_enabled());
	test_skip_if(known_failure_on_android);

	ticker_t *decay_ticker;
	unsigned tick0, tick1, arena_ind;
	size_t sz, large0;
	void *p;

	sz = sizeof(size_t);
	assert_d_eq(mallctl("arenas.lextent.0.size", (void *)&large0, &sz, NULL,
	    0), 0, "Unexpected mallctl failure");

	/* Set up a manually managed arena for test. */
	arena_ind = do_arena_create(0, 0);

	/* Migrate to the new arena, and get the ticker. */
	unsigned old_arena_ind;
	size_t sz_arena_ind = sizeof(old_arena_ind);
	assert_d_eq(mallctl("thread.arena", (void *)&old_arena_ind,
	    &sz_arena_ind, (void *)&arena_ind, sizeof(arena_ind)), 0,
	    "Unexpected mallctl() failure");
	decay_ticker = decay_ticker_get(tsd_fetch(), arena_ind);
	assert_ptr_not_null(decay_ticker,
	    "Unexpected failure getting decay ticker");

	/*
	 * Test the standard APIs using a large size class, since we can't
	 * control tcache interactions for small size classes (except by
	 * completely disabling tcache for the entire test program).
	 */

	/* malloc(). */
	tick0 = ticker_read(decay_ticker);
	p = malloc(large0);
	assert_ptr_not_null(p, "Unexpected malloc() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during malloc()");
	/* free(). */
	tick0 = ticker_read(decay_ticker);
	free(p);
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during free()");

	/* calloc(). */
	tick0 = ticker_read(decay_ticker);
	p = calloc(1, large0);
	assert_ptr_not_null(p, "Unexpected calloc() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during calloc()");
	free(p);

	/* posix_memalign(). */
	tick0 = ticker_read(decay_ticker);
	assert_d_eq(posix_memalign(&p, sizeof(size_t), large0), 0,
	    "Unexpected posix_memalign() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0,
	    "Expected ticker to tick during posix_memalign()");
	free(p);

	/* aligned_alloc(). */
	tick0 = ticker_read(decay_ticker);
	p = aligned_alloc(sizeof(size_t), large0);
	assert_ptr_not_null(p, "Unexpected aligned_alloc() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0,
	    "Expected ticker to tick during aligned_alloc()");
	free(p);

	/* realloc(). */
	/* Allocate. */
	tick0 = ticker_read(decay_ticker);
	p = realloc(NULL, large0);
	assert_ptr_not_null(p, "Unexpected realloc() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during realloc()");
	/* Reallocate. */
	tick0 = ticker_read(decay_ticker);
	p = realloc(p, large0);
	assert_ptr_not_null(p, "Unexpected realloc() failure");
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during realloc()");
	/* Deallocate. */
	tick0 = ticker_read(decay_ticker);
	realloc(p, 0);
	tick1 = ticker_read(decay_ticker);
	assert_u32_ne(tick1, tick0, "Expected ticker to tick during realloc()");

	/*
	 * Test the *allocx() APIs using large and small size classes, with
	 * tcache explicitly disabled.
	 */
	{
		unsigned i;
		size_t allocx_sizes[2];
		allocx_sizes[0] = large0;
		allocx_sizes[1] = 1;

		for (i = 0; i < sizeof(allocx_sizes) / sizeof(size_t); i++) {
			sz = allocx_sizes[i];

			/* mallocx(). */
			tick0 = ticker_read(decay_ticker);
			p = mallocx(sz, MALLOCX_TCACHE_NONE);
			assert_ptr_not_null(p, "Unexpected mallocx() failure");
			tick1 = ticker_read(decay_ticker);
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during mallocx() (sz=%zu)",
			    sz);
			/* rallocx(). */
			tick0 = ticker_read(decay_ticker);
			p = rallocx(p, sz, MALLOCX_TCACHE_NONE);
			assert_ptr_not_null(p, "Unexpected rallocx() failure");
			tick1 = ticker_read(decay_ticker);
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during rallocx() (sz=%zu)",
			    sz);
			/* xallocx(). */
			tick0 = ticker_read(decay_ticker);
			xallocx(p, sz, 0, MALLOCX_TCACHE_NONE);
			tick1 = ticker_read(decay_ticker);
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during xallocx() (sz=%zu)",
			    sz);
			/* dallocx(). */
			tick0 = ticker_read(decay_ticker);
			dallocx(p, MALLOCX_TCACHE_NONE);
			tick1 = ticker_read(decay_ticker);
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during dallocx() (sz=%zu)",
			    sz);
			/* sdallocx(). */
			p = mallocx(sz, MALLOCX_TCACHE_NONE);
			assert_ptr_not_null(p, "Unexpected mallocx() failure");
			tick0 = ticker_read(decay_ticker);
			sdallocx(p, sz, MALLOCX_TCACHE_NONE);
			tick1 = ticker_read(decay_ticker);
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during sdallocx() "
			    "(sz=%zu)", sz);
		}
	}

	/*
	 * Test tcache fill/flush interactions for large and small size classes,
	 * using an explicit tcache.
	 */
	unsigned tcache_ind, i;
	size_t tcache_sizes[2];
	tcache_sizes[0] = large0;
	tcache_sizes[1] = 1;

	size_t tcache_max, sz_tcache_max;
	sz_tcache_max = sizeof(tcache_max);
	assert_d_eq(mallctl("arenas.tcache_max", (void *)&tcache_max,
	    &sz_tcache_max, NULL, 0), 0, "Unexpected mallctl() failure");

	sz = sizeof(unsigned);
	assert_d_eq(mallctl("tcache.create", (void *)&tcache_ind, &sz,
	    NULL, 0), 0, "Unexpected mallctl failure");

	for (i = 0; i < sizeof(tcache_sizes) / sizeof(size_t); i++) {
		sz = tcache_sizes[i];

		/* tcache fill. */
		tick0 = ticker_read(decay_ticker);
		p = mallocx(sz, MALLOCX_TCACHE(tcache_ind));
		assert_ptr_not_null(p, "Unexpected mallocx() failure");
		tick1 = ticker_read(decay_ticker);
		assert_u32_ne(tick1, tick0,
		    "Expected ticker to tick during tcache fill "
		    "(sz=%zu)", sz);
		/* tcache flush. */
		dallocx(p, MALLOCX_TCACHE(tcache_ind));
		tick0 = ticker_read(decay_ticker);
		assert_d_eq(mallctl("tcache.flush", NULL, NULL,
		    (void *)&tcache_ind, sizeof(unsigned)), 0,
		    "Unexpected mallctl failure");
		tick1 = ticker_read(decay_ticker);

		/* Will only tick if it's in tcache. */
		if (sz <= tcache_max) {
			assert_u32_ne(tick1, tick0,
			    "Expected ticker to tick during tcache "
			    "flush (sz=%zu)", sz);
		} else {
			assert_u32_eq(tick1, tick0,
			    "Unexpected ticker tick during tcache "
			    "flush (sz=%zu)", sz);
		}
>>>>>>> ba151f03a6e345b1159b6e7798dbe89112baa650
	}
}
TEST_END

TEST_BEGIN(test_decay_npages_purge_in) {
	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	uint64_t decay_ms = 1000;
	nstime_t decay_nstime;
	nstime_init(&decay_nstime, decay_ms * 1000 * 1000);
	expect_false(decay_init(&decay, &curtime, (ssize_t)decay_ms),
	    "Failed to initialize decay");

	size_t new_pages = 100;

	nstime_t time;
	nstime_copy(&time, &decay_nstime);
	expect_u64_eq(decay_npages_purge_in(&decay, &time, new_pages),
	    new_pages, "Not all pages are expected to decay in decay_ms");

	nstime_init(&time, 0);
	expect_u64_eq(decay_npages_purge_in(&decay, &time, new_pages), 0,
	    "More than zero pages are expected to instantly decay");

	nstime_copy(&time, &decay_nstime);
	nstime_idivide(&time, 2);
	expect_u64_eq(decay_npages_purge_in(&decay, &time, new_pages),
	    new_pages / 2, "Not half of pages decay in half the decay period");
}
TEST_END

TEST_BEGIN(test_decay_maybe_advance_epoch) {
	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	uint64_t decay_ms = 1000;

	bool err = decay_init(&decay, &curtime, (ssize_t)decay_ms);
	expect_false(err, "");

	bool advanced;
	advanced = decay_maybe_advance_epoch(&decay, &curtime, 0);
	expect_false(advanced, "Epoch advanced while time didn't");

	nstime_t interval;
	nstime_init(&interval, decay_epoch_duration_ns(&decay));

	nstime_add(&curtime, &interval);
	advanced = decay_maybe_advance_epoch(&decay, &curtime, 0);
	expect_false(advanced, "Epoch advanced after first interval");

	nstime_add(&curtime, &interval);
	advanced = decay_maybe_advance_epoch(&decay, &curtime, 0);
	expect_true(advanced, "Epoch didn't advance after two intervals");
}
TEST_END

TEST_BEGIN(test_decay_empty) {
	/* If we never have any decaying pages, npages_limit should be 0. */
	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	uint64_t decay_ms = 1000;
	uint64_t decay_ns = decay_ms * 1000 * 1000;

	bool err = decay_init(&decay, &curtime, (ssize_t)decay_ms);
	assert_false(err, "");

	uint64_t time_between_calls = decay_epoch_duration_ns(&decay) / 5;
	int nepochs = 0;
	for (uint64_t i = 0; i < decay_ns / time_between_calls * 10; i++) {
		size_t dirty_pages = 0;
		nstime_init(&curtime, i * time_between_calls);
		bool epoch_advanced = decay_maybe_advance_epoch(&decay,
		    &curtime, dirty_pages);
		if (epoch_advanced) {
			nepochs++;
			expect_zu_eq(decay_npages_limit_get(&decay), 0,
			    "Unexpectedly increased npages_limit");
		}
	}
	expect_d_gt(nepochs, 0, "Epochs never advanced");
}
TEST_END

/*
 * Verify that npages_limit correctly decays as the time goes.
 *
 * During first 'nepoch_init' epochs, add new dirty pages.
 * After that, let them decay and verify npages_limit decreases.
 * Then proceed with another 'nepoch_init' epochs and check that
 * all dirty pages are flushed out of backlog, bringing npages_limit
 * down to zero.
 */
TEST_BEGIN(test_decay) {
	const uint64_t nepoch_init = 10;

	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	uint64_t decay_ms = 1000;
	uint64_t decay_ns = decay_ms * 1000 * 1000;

	bool err = decay_init(&decay, &curtime, (ssize_t)decay_ms);
	assert_false(err, "");

	expect_zu_eq(decay_npages_limit_get(&decay), 0,
	    "Empty decay returned nonzero npages_limit");

	nstime_t epochtime;
	nstime_init(&epochtime, decay_epoch_duration_ns(&decay));

	const size_t dirty_pages_per_epoch = 1000;
	size_t dirty_pages = 0;
	uint64_t epoch_ns = decay_epoch_duration_ns(&decay);
	bool epoch_advanced = false;

	/* Populate backlog with some dirty pages */
	for (uint64_t i = 0; i < nepoch_init; i++) {
		nstime_add(&curtime, &epochtime);
		dirty_pages += dirty_pages_per_epoch;
		epoch_advanced |= decay_maybe_advance_epoch(&decay, &curtime,
		    dirty_pages);
	}
	expect_true(epoch_advanced, "Epoch never advanced");

	size_t npages_limit = decay_npages_limit_get(&decay);
	expect_zu_gt(npages_limit, 0, "npages_limit is incorrectly equal "
	    "to zero after dirty pages have been added");

	/* Keep dirty pages unchanged and verify that npages_limit decreases */
	for (uint64_t i = nepoch_init; i * epoch_ns < decay_ns; ++i) {
		nstime_add(&curtime, &epochtime);
		epoch_advanced = decay_maybe_advance_epoch(&decay, &curtime,
				    dirty_pages);
		if (epoch_advanced) {
			size_t npages_limit_new = decay_npages_limit_get(&decay);
			expect_zu_lt(npages_limit_new, npages_limit,
			    "napges_limit failed to decay");

			npages_limit = npages_limit_new;
		}
	}

	expect_zu_gt(npages_limit, 0, "npages_limit decayed to zero earlier "
	    "than decay_ms since last dirty page was added");

	/* Completely push all dirty pages out of the backlog */
	epoch_advanced = false;
	for (uint64_t i = 0; i < nepoch_init; i++) {
		nstime_add(&curtime, &epochtime);
		epoch_advanced |= decay_maybe_advance_epoch(&decay, &curtime,
		    dirty_pages);
	}
	expect_true(epoch_advanced, "Epoch never advanced");

	npages_limit = decay_npages_limit_get(&decay);
	expect_zu_eq(npages_limit, 0, "npages_limit didn't decay to 0 after "
	    "decay_ms since last bump in dirty pages");
}
TEST_END

TEST_BEGIN(test_decay_ns_until_purge) {
	const uint64_t nepoch_init = 10;

	decay_t decay;
	memset(&decay, 0, sizeof(decay));

	nstime_t curtime;
	nstime_init(&curtime, 0);

	uint64_t decay_ms = 1000;
	uint64_t decay_ns = decay_ms * 1000 * 1000;

	bool err = decay_init(&decay, &curtime, (ssize_t)decay_ms);
	assert_false(err, "");

	nstime_t epochtime;
	nstime_init(&epochtime, decay_epoch_duration_ns(&decay));

	uint64_t ns_until_purge_empty = decay_ns_until_purge(&decay, 0, 0);
	expect_u64_eq(ns_until_purge_empty, DECAY_UNBOUNDED_TIME_TO_PURGE,
	    "Failed to return unbounded wait time for zero threshold");

	const size_t dirty_pages_per_epoch = 1000;
	size_t dirty_pages = 0;
	bool epoch_advanced = false;
	for (uint64_t i = 0; i < nepoch_init; i++) {
		nstime_add(&curtime, &epochtime);
		dirty_pages += dirty_pages_per_epoch;
		epoch_advanced |= decay_maybe_advance_epoch(&decay, &curtime,
		    dirty_pages);
	}
	expect_true(epoch_advanced, "Epoch never advanced");

	uint64_t ns_until_purge_all = decay_ns_until_purge(&decay,
	    dirty_pages, dirty_pages);
	expect_u64_ge(ns_until_purge_all, decay_ns,
	    "Incorrectly calculated time to purge all pages");

	uint64_t ns_until_purge_none = decay_ns_until_purge(&decay,
	    dirty_pages, 0);
	expect_u64_eq(ns_until_purge_none, decay_epoch_duration_ns(&decay) * 2,
	    "Incorrectly calculated time to purge 0 pages");

	uint64_t npages_threshold = dirty_pages / 2;
	uint64_t ns_until_purge_half = decay_ns_until_purge(&decay,
	    dirty_pages, npages_threshold);

	nstime_t waittime;
	nstime_init(&waittime, ns_until_purge_half);
	nstime_add(&curtime, &waittime);

	decay_maybe_advance_epoch(&decay, &curtime, dirty_pages);
	size_t npages_limit = decay_npages_limit_get(&decay);
	expect_zu_lt(npages_limit, dirty_pages,
	    "npages_limit failed to decrease after waiting");
	size_t expected = dirty_pages - npages_limit;
	int deviation = abs((int)expected - (int)(npages_threshold));
	expect_d_lt(deviation, (int)(npages_threshold / 2),
	    "After waiting, number of pages is out of the expected interval "
	    "[0.5 * npages_threshold .. 1.5 * npages_threshold]");
}
TEST_END

int
main(void) {
	return test(
	    test_decay_init,
	    test_decay_ms_valid,
	    test_decay_npages_purge_in,
	    test_decay_maybe_advance_epoch,
	    test_decay_empty,
	    test_decay,
	    test_decay_ns_until_purge);
}
