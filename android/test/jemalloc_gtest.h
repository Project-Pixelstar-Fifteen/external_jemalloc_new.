// Copyright (C) 2021 The Android Open Source Project
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <stdbool.h>
#include <sys/wait.h>

// jemalloc unit tests exit with 0 for success, 1 for success with skipped
// tests, and 2 for failure.
static inline bool testResultPredicate(int exit_status) {
  return exit_status == 0 || exit_status == 1;
}

static const bool known_failure_on_android = true;
