/*
 * Copyright (C) 2014 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

void je_stats_arena(size_t arena_index, void (*callback)(size_t, size_t, size_t)) {
  malloc_mutex_lock(TSDN_NULL, &arenas_lock);
  arena_t* arena = atomic_load_p(&arenas[arena_index], ATOMIC_ACQUIRE);
  if (arena == NULL) {
    malloc_mutex_unlock(TSDN_NULL, &arenas_lock);
    return;
  }

  for (unsigned j = 0; j < NBINS; j++) {
    bin_t* bin = &arena->bins[j];

    /* NOTE: This includes allocations cached on every thread. */
    malloc_mutex_lock(TSDN_NULL, &bin->lock);
    callback(j, bin_infos[j].reg_size, bin->stats.curregs);
    malloc_mutex_unlock(TSDN_NULL, &bin->lock);
  }

  /* Accumulate the large allocation stats.
   * Do not include stats.allocated_large, it is only updated by
   * arena_stats_merge, and would include the data counted below.
   */
  for (unsigned j = NBINS; j < NSIZES; j++) {
    /* Read ndalloc first so that we guarantee nmalloc >= ndalloc. */
    uint64_t ndalloc = arena_stats_read_u64(TSDN_NULL, &arena->stats, &arena->stats.lstats[j - NBINS].ndalloc);
    uint64_t nmalloc = arena_stats_read_u64(TSDN_NULL, &arena->stats, &arena->stats.lstats[j - NBINS].nmalloc);
    callback(j, sz_index2size(j), (size_t)(nmalloc - ndalloc));
  }
  malloc_mutex_unlock(TSDN_NULL, &arenas_lock);
}
