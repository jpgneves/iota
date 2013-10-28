#!/bin/sh

set -e

OTP_RELEASE=$(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]),halt(0).')
MAJOR=$(echo $OTP_RELEASE | sed -ne 's/\(R.*\)[A-C].*$/\1/gp')
MINOR=$(echo $OTP_RELEASE | sed -ne 's/R.*\([A-C].*\)$/\1/gp')

case $MAJOR in
    R14)
        # R14 is very slow creating the PLT, we just ignore dialyzer for it
        echo "** WARNING not running dialyzer for R14!"
        ;;
    R15)
        # -Werror_handling yields unwanted warnings in R15
        dialyzer --plt $DIALYZER_PLT $PRODUCTION_BEAMS \
            -Wunmatched_returns \
            -Wrace_conditions
        ;;
    *)
        dialyzer --plt $DIALYZER_PLT $PRODUCTION_BEAMS \
            -Wunmatched_returns \
            -Werror_handling \
            -Wrace_conditions
        ;;
esac
