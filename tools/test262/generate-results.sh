#!/bin/sh

if [ "$#" -ne 3 ]; then
  echo "usage: generate-results.sh /path/to/kiesel /path/to/test262 'test/**/*.js'"
  exit 1
fi

kiesel_path="$(realpath "$1")"
test262_dir="$(realpath "$2")"
test262_glob="$3"

# We have to cd to ensure all paths in the output start with 'test/' and not
# something like '../test262/test/'
cd "${test262_dir}"

# Make sure the default locale is 'und', the CLI tries to infer it from $LANG
export LANG=''

test262-harness \
  --host-type=kiesel \
  --host-path="${kiesel_path}" \
  --test262-dir="${test262_dir}" \
  --reporter=json --reporter-keys=file,result \
  --timeout=20000 \
  --threads="$(nproc)" \
  "${test262_glob}" \
| jq -r '
  sort_by(
    .file,
    # Ensure that the final result is always FAIL when results for the same
    # file differ (strict/non-strict) by doing a secondary sort on the result
    (if .result.pass then 0 else 1 end))
  | map(
    # Some of the SpiderMonkey staging tests are really slow which leads to
    # spurious timeouts and mismatches between local runs and CI.
    # There is no good way to exclude subdirectories so we filter them here.
    # The only other solution would be to set the timeout very high or very low,
    # which other tests would suffer from.
    select(.file | startswith("test/staging/sm/") | not) | {
    (.file): (if .result.pass then "PASS" else "FAIL" end)
  })
  | add'
