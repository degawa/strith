module strith_parameter
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    integer(int32), public, parameter :: sign_index = 1
        !! the position of the sign in a string
    character(1), public, parameter :: plus_sign = "+"
        !! the plus sign
    character(1), public, parameter :: minus_sign = "-"
        !! the minus sign
    character(1), public, parameter :: sign_symbols(*) = [plus_sign, minus_sign]
        !! the sign symbols

    character(1), public, parameter :: number(0:*) = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        !! characters composing a string
    character(*), public, parameter :: weights_of_digits(0:*) = ["+000000000000000000000000000000000000001", &
                                                                 "+000000000000000000000000000000000000002", &
                                                                 "+000000000000000000000000000000000000004", &
                                                                 "+000000000000000000000000000000000000008", &
                                                                 "+000000000000000000000000000000000000016", &
                                                                 "+000000000000000000000000000000000000032", &
                                                                 "+000000000000000000000000000000000000064", &
                                                                 "+000000000000000000000000000000000000128", &
                                                                 "+000000000000000000000000000000000000256", &
                                                                 "+000000000000000000000000000000000000512", &
                                                                 "+000000000000000000000000000000000001024", &
                                                                 "+000000000000000000000000000000000002048", &
                                                                 "+000000000000000000000000000000000004096", &
                                                                 "+000000000000000000000000000000000008192", &
                                                                 "+000000000000000000000000000000000016384", &
                                                                 "+000000000000000000000000000000000032768", &
                                                                 "+000000000000000000000000000000000065536", &
                                                                 "+000000000000000000000000000000000131072", &
                                                                 "+000000000000000000000000000000000262144", &
                                                                 "+000000000000000000000000000000000524288", &
                                                                 "+000000000000000000000000000000001048576", &
                                                                 "+000000000000000000000000000000002097152", &
                                                                 "+000000000000000000000000000000004194304", &
                                                                 "+000000000000000000000000000000008388608", &
                                                                 "+000000000000000000000000000000016777216", &
                                                                 "+000000000000000000000000000000033554432", &
                                                                 "+000000000000000000000000000000067108864", &
                                                                 "+000000000000000000000000000000134217728", &
                                                                 "+000000000000000000000000000000268435456", &
                                                                 "+000000000000000000000000000000536870912", &
                                                                 "+000000000000000000000000000001073741824", &
                                                                 "+000000000000000000000000000002147483648", &
                                                                 "+000000000000000000000000000004294967296", &
                                                                 "+000000000000000000000000000008589934592", &
                                                                 "+000000000000000000000000000017179869184", &
                                                                 "+000000000000000000000000000034359738368", &
                                                                 "+000000000000000000000000000068719476736", &
                                                                 "+000000000000000000000000000137438953472", &
                                                                 "+000000000000000000000000000274877906944", &
                                                                 "+000000000000000000000000000549755813888", &
                                                                 "+000000000000000000000000001099511627776", &
                                                                 "+000000000000000000000000002199023255552", &
                                                                 "+000000000000000000000000004398046511104", &
                                                                 "+000000000000000000000000008796093022208", &
                                                                 "+000000000000000000000000017592186044416", &
                                                                 "+000000000000000000000000035184372088832", &
                                                                 "+000000000000000000000000070368744177664", &
                                                                 "+000000000000000000000000140737488355328", &
                                                                 "+000000000000000000000000281474976710656", &
                                                                 "+000000000000000000000000562949953421312", &
                                                                 "+000000000000000000000001125899906842624", &
                                                                 "+000000000000000000000002251799813685248", &
                                                                 "+000000000000000000000004503599627370496", &
                                                                 "+000000000000000000000009007199254740992", &
                                                                 "+000000000000000000000018014398509481984", &
                                                                 "+000000000000000000000036028797018963968", &
                                                                 "+000000000000000000000072057594037927936", &
                                                                 "+000000000000000000000144115188075855872", &
                                                                 "+000000000000000000000288230376151711744", &
                                                                 "+000000000000000000000576460752303423488", &
                                                                 "+000000000000000000001152921504606846976", &
                                                                 "+000000000000000000002305843009213693952", &
                                                                 "+000000000000000000004611686018427387904", &
                                                                 "+000000000000000000009223372036854775808", &
                                                                 "+000000000000000000018446744073709551616", &
                                                                 "+000000000000000000036893488147419103232", &
                                                                 "+000000000000000000073786976294838206464", &
                                                                 "+000000000000000000147573952589676412928", &
                                                                 "+000000000000000000295147905179352825856", &
                                                                 "+000000000000000000590295810358705651712", &
                                                                 "+000000000000000001180591620717411303424", &
                                                                 "+000000000000000002361183241434822606848", &
                                                                 "+000000000000000004722366482869645213696", &
                                                                 "+000000000000000009444732965739290427392", &
                                                                 "+000000000000000018889465931478580854784", &
                                                                 "+000000000000000037778931862957161709568", &
                                                                 "+000000000000000075557863725914323419136", &
                                                                 "+000000000000000151115727451828646838272", &
                                                                 "+000000000000000302231454903657293676544", &
                                                                 "+000000000000000604462909807314587353088", &
                                                                 "+000000000000001208925819614629174706176", &
                                                                 "+000000000000002417851639229258349412352", &
                                                                 "+000000000000004835703278458516698824704", &
                                                                 "+000000000000009671406556917033397649408", &
                                                                 "+000000000000019342813113834066795298816", &
                                                                 "+000000000000038685626227668133590597632", &
                                                                 "+000000000000077371252455336267181195264", &
                                                                 "+000000000000154742504910672534362390528", &
                                                                 "+000000000000309485009821345068724781056", &
                                                                 "+000000000000618970019642690137449562112", &
                                                                 "+000000000001237940039285380274899124224", &
                                                                 "+000000000002475880078570760549798248448", &
                                                                 "+000000000004951760157141521099596496896", &
                                                                 "+000000000009903520314283042199192993792", &
                                                                 "+000000000019807040628566084398385987584", &
                                                                 "+000000000039614081257132168796771975168", &
                                                                 "+000000000079228162514264337593543950336", &
                                                                 "+000000000158456325028528675187087900672", &
                                                                 "+000000000316912650057057350374175801344", &
                                                                 "+000000000633825300114114700748351602688", &
                                                                 "+000000001267650600228229401496703205376", &
                                                                 "+000000002535301200456458802993406410752", &
                                                                 "+000000005070602400912917605986812821504", &
                                                                 "+000000010141204801825835211973625643008", &
                                                                 "+000000020282409603651670423947251286016", &
                                                                 "+000000040564819207303340847894502572032", &
                                                                 "+000000081129638414606681695789005144064", &
                                                                 "+000000162259276829213363391578010288128", &
                                                                 "+000000324518553658426726783156020576256", &
                                                                 "+000000649037107316853453566312041152512", &
                                                                 "+000001298074214633706907132624082305024", &
                                                                 "+000002596148429267413814265248164610048", &
                                                                 "+000005192296858534827628530496329220096", &
                                                                 "+000010384593717069655257060992658440192", &
                                                                 "+000020769187434139310514121985316880384", &
                                                                 "+000041538374868278621028243970633760768", &
                                                                 "+000083076749736557242056487941267521536", &
                                                                 "+000166153499473114484112975882535043072", &
                                                                 "+000332306998946228968225951765070086144", &
                                                                 "+000664613997892457936451903530140172288", &
                                                                 "+001329227995784915872903807060280344576", &
                                                                 "+002658455991569831745807614120560689152", &
                                                                 "+005316911983139663491615228241121378304", &
                                                                 "+010633823966279326983230456482242756608", &
                                                                 "+021267647932558653966460912964485513216", &
                                                                 "+042535295865117307932921825928971026432", &
                                                                 "+085070591730234615865843651857942052864", &
                                                                 "+170141183460469231731687303715884105728" &
                                                                 ]
        !! weights of corresponding digits
    integer(int32), public, parameter :: digits = len(weights_of_digits(0))
        !! length of a string
    integer(int32), public, parameter :: leftmost_digit_index = 2
        !! the leftmost position of a number in a string
    integer(int32), public, parameter :: rightmost_digit_index = digits
        !! the rightmost position of a number in a string

    integer(int32), public, parameter :: increment_from_right_to_left = sign(1, leftmost_digit_index - rightmost_digit_index)
        !! do-loop counter increment from the rightmost index to the leftmost
    integer(int32), public, parameter :: increment_from_left_to_right = sign(1, rightmost_digit_index - leftmost_digit_index)
        !! do-loop counter increment from the leftmost index to the rightmost

    character(len=digits), public, parameter :: zero = "+"//repeat("0", digits - 1)
        !! the constant representing 0
end module strith_parameter
