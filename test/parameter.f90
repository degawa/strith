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
end program test_parameter
