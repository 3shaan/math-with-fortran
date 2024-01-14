program name
    implicit none

    integer :: k, n
    real :: a, b, h, sum, s13

    a = 0.0
    b = 6.0
    n = 12

    h = (b - a) / real(n, 8)

    sum = f(a) + f(b)

    do k = 1, n - 1
        if (mod(k, 2) == 0) then
            sum = sum + 2.0 * f(a + k * h)
        else
            sum = sum + 4.0 * f(a + k * h)
        endif
    end do

    s13 = (sum + f(a + n * h)) * h / 3.0

    print*, s13

contains

    real function f(x)
        real, intent(in) :: x
        f = x
    end function f

end program name
