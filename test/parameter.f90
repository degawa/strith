program test_parameter
    use :: strith_parameter
    implicit none

    if (zero == "+000000000000000000000000000000000000000") then
        print '(A)', "✅PASSED: parameter zero is +000000000000000000000000000000000000000"
    else
        print '(A)', "🔥FAILED: parameter zero is +000000000000000000000000000000000000000 "// &
            "but got "//zero
    end if

    if (one == "+000000000000000000000000000000000000001") then
        print '(A)', "✅PASSED: parameter one is +000000000000000000000000000000000000001"
    else
        print '(A)', "🔥FAILED: parameter one is +000000000000000000000000000000000000001 "// &
            "but got "//one
    end if

    if (int8_max == "+000000000000000000000000000000000000127") then
        print '(A)', "✅PASSED: parameter int8_max is +000000000000000000000000000000000000127"
    else
        print '(A)', "🔥FAILED: parameter int8_max is +000000000000000000000000000000000000127 "// &
            "but got "//int8_max
    end if

    if (int8_min == "-000000000000000000000000000000000000128") then
        print '(A)', "✅PASSED: parameter int8_min is -000000000000000000000000000000000000128"
    else
        print '(A)', "🔥FAILED: parameter int8_min is -000000000000000000000000000000000000128 "// &
            "but got "//int8_min
    end if

    if (uint8_max == "+000000000000000000000000000000000000256") then
        print '(A)', "✅PASSED: parameter uint8_max is +000000000000000000000000000000000000256"
    else
        print '(A)', "🔥FAILED: parameter uint8_max is +000000000000000000000000000000000000256 "// &
            "but got "//uint8_max
    end if

    if (int16_max == "+000000000000000000000000000000000032767") then
        print '(A)', "✅PASSED: parameter int16_max is +000000000000000000000000000000000032767"
    else
        print '(A)', "🔥FAILED: parameter int16_max is +000000000000000000000000000000000032767 "// &
            "but got "//int16_max
    end if

    if (int16_min == "-000000000000000000000000000000000032768") then
        print '(A)', "✅PASSED: parameter int16_min is -000000000000000000000000000000000032768"
    else
        print '(A)', "🔥FAILED: parameter int16_min is -000000000000000000000000000000000032768 "// &
            "but got "//int16_min
    end if

    if (uint16_max == "+000000000000000000000000000000000065536") then
        print '(A)', "✅PASSED: parameter uint16_max is +000000000000000000000000000000000065536"
    else
        print '(A)', "🔥FAILED: parameter uint16_max is +000000000000000000000000000000000065536 "// &
            "but got "//uint16_max
    end if

    if (int32_max == "+000000000000000000000000000002147483647") then
        print '(A)', "✅PASSED: parameter int32_max is +000000000000000000000000000002147483647"
    else
        print '(A)', "🔥FAILED: parameter int32_max is +000000000000000000000000000002147483647 "// &
            "but got "//int32_max
    end if

    if (int32_min == "-000000000000000000000000000002147483648") then
        print '(A)', "✅PASSED: parameter int32_min is -000000000000000000000000000002147483648"
    else
        print '(A)', "🔥FAILED: parameter int32_min is -000000000000000000000000000002147483648 "// &
            "but got "//int32_min
    end if

    if (uint32_max == "+000000000000000000000000000004294967296") then
        print '(A)', "✅PASSED: parameter uint32_max is +000000000000000000000000000004294967296"
    else
        print '(A)', "🔥FAILED: parameter uint32_max is +000000000000000000000000000004294967296 "// &
            "but got "//uint32_max
    end if

    if (int64_max == "+000000000000000000009223372036854775807") then
        print '(A)', "✅PASSED: parameter int64_max is +000000000000000000009223372036854775807"
    else
        print '(A)', "🔥FAILED: parameter int64_max is +000000000000000000009223372036854775807 "// &
            "but got "//int64_max
    end if

    if (int64_min == "-000000000000000000009223372036854775808") then
        print '(A)', "✅PASSED: parameter int64_min is -000000000000000000009223372036854775808"
    else
        print '(A)', "🔥FAILED: parameter int64_min is -000000000000000000009223372036854775808 "// &
            "but got "//int64_min
    end if

    if (uint64_max == "+000000000000000000018446744073709551616") then
        print '(A)', "✅PASSED: parameter uint64_max is +000000000000000000018446744073709551616"
    else
        print '(A)', "🔥FAILED: parameter uint64_max is +000000000000000000018446744073709551616 "// &
            "but got "//uint64_max
    end if

    if (int128_max == "+170141183460469231731687303715884105727") then
        print '(A)', "✅PASSED: parameter int128_max is +170141183460469231731687303715884105727"
    else
        print '(A)', "🔥FAILED: parameter int128_max is +170141183460469231731687303715884105727 "// &
            "but got "//int128_max
    end if

    if (int128_min == "-170141183460469231731687303715884105728") then
        print '(A)', "✅PASSED: parameter int128_min is -170141183460469231731687303715884105728"
    else
        print '(A)', "🔥FAILED: parameter int128_min is -170141183460469231731687303715884105728 "// &
            "but got "//int128_min
    end if

    if (uint128_max == "+340282366920938463463374607431768211456") then
        print '(A)', "✅PASSED: parameter uint128_max is +340282366920938463463374607431768211456"
    else
        print '(A)', "🔥FAILED: parameter uint128_max is +340282366920938463463374607431768211456 "// &
            "but got "//uint128_max
    end if
end program test_parameter
