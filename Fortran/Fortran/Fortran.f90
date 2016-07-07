   
module mymod

contains
    subroutine  util_integer(ix, n, work)
        integer :: ix, n
         !DEC$ ATTRIBUTES NO_ARG_CHECK :: work
        integer :: work(1)
        do i=1,n
            work(ix+i-1)=i
        enddo 
    end subroutine 
    
    subroutine util_real(ix, n, work)
        integer:: ix, n
         !DEC$ ATTRIBUTES NO_ARG_CHECK :: work
        real ::work(1)
        do i=1,n
            work(ix+i-1)=0.3
        enddo   
    end subroutine
    
end module 
    
program Fortran
    
    use mymod
    
    implicit none


    ! Variables

    real work(100);
    
    call util_integer(1,10, work(1))
    call util_real(20,10,work(1))
    
    ! Body of Fortran
    print *, 'Hello World'
       
    
end program Fortran

 