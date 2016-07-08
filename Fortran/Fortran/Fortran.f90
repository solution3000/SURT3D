   
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
    
    subroutine test100(n, x, y, z, p,q,r)
    integer::n
    !此处声明正确，编译不会报错，计算正确
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: x,y,z,p,q,r
    real,dimension(n)::x,y,z,p,q,r
    do i=1,n
       p(i)=x(i)+y(i)+z(i)
       q(i)=x(i)*y(i)*z(i)
       r(i)=x(i)*y(i)
    end do  
    end subroutine 

end module 

    
program Fortran
    use mymod
    implicit none

    real work(100);
    real ::x,y,z, p,q,r
    
    call util_integer(1,10, work(1))
    call util_real(20,10,work(1))
    
    print *, 'Hello World'

    !编译报错
    !call test2(10,1.0,2.0,3.0,4.0,5.0,6.0)
    
    !调用是对的
    call test100(1,1.0,2.0,3.0,p,q,r)
    write(*,*) p, q, r
    
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
    
 