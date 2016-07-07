   
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
       
    call test2(10,1.0,2.0,3.0,4.0,5.0,6.0)
    
    end program Fortran

    
    subroutine test1(n, pz,px,py, dvdz,dvdx,dvdy)
        integer :: n
        real :: pz(1),px(1),py(1)
        real :: dvdz(1),dvdx(1), dvdy(1)
    end subroutine 
    
    subroutine  test2(n, pz, px, py, dvdz, dvdx, dvdy)
        integer :: n
        real ::pz(*),px(*),py(*)
        real :: dvdz(*), dvdx(*), dvdy(*)
    end subroutine 
    
    subroutine test3(n, pz, px, py, dvdz, dvdx, dvdy)
        integer :: n
        real :: pz(n),px(n),py(n)
        real :: dvdz(n), dvdx(n), dvdy(n)
    end subroutine 
    
 