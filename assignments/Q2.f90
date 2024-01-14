!  Q2 -->   A function f(x) is defined by f(x) =(-x if x≤0 ; x if 0 <x <
!  1; 1-x if x≥1} Write a Fortran Program segment to evaluate the
!  above function


! This is a piecewise math where i have to write a program to evaluate this.

program piecewise
    implicit none

    real :: x, f
    print*, "Enter the value of x:"
    read(*,*) x

    if (x .le. 0.0) then
        f = x
    elseif (x .gt. 0.0 .and. x .lt. 1.0) then
        f = x
    elseif (x .ge. 1.0) then
        f = 1.0 - x
    end if

    write(*,*) 'f(',x,') = ', f

end program piecewise


