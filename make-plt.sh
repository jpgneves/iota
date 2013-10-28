#!/bin/sh

OTP_RELEASE=$(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]),halt(0).')
MAJOR=$(echo $OTP_RELEASE | sed -ne 's/\(R.*\)[A-C].*$/\1/gp')
MINOR=$(echo $OTP_RELEASE | sed -ne 's/R.*\([A-C].*\)$/\1/gp')

case $MAJOR in
    R14)
        # R14 is very slow creating the PLT, we just ignore dialyzer for it
        echo "** WARNING not creating a PLT for R14!"
        touch $DIALYZER_PLT
        ;;
    *)
        dialyzer --build_plt --output_plt $DIALYZER_PLT \
            lib/*/ebin \
            --apps stdlib kernel tools syntax_tools compiler erts \
        ;;
esac
