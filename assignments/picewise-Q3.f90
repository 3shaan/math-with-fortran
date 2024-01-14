! A function is defined as follows : f(x)={1-x**2,x<0
! ;1+2x,0≤x<1 3+1/x,X>1 }If x (-10, 10] then find the velue of f(x)
! by using fortran for each x= 0.5


program picewise2

    implicit none

    real :: x, result ;

    do x = -10.0 , 10.0, 0.5 ! this is loop. it will start from -10 and end 10 and every loop it will increase by 0.5 ;

        result = evaluateFunc(x); ! pass the x to the functions which will do the work there

        write(*,*) 'f(', x, ')' , result;
    end do 


    contains 


    !this is the function where picewise function i write
    function evaluateFunc(input) result(output)
        real, intent(in) :: input
        real :: output

        if( input < 0 ) then 
            output = 1.0 - input**2
        elseif (input >= 0 .and. x < 1 ) then
            output = 1.0 + 2.0 * input
        elseif (input > 1.0) then
            output = 3.0 + 1.0 / input
        end if 

        
    end function evaluateFunc
    

end program picewise2
