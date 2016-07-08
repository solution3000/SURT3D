   
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


module cstr

    contains

    function c_to_f_string(s) result(str)
        use iso_c_binding
        character(kind=c_char,len=1), intent(in) :: s(*)
        character(len=:), allocatable :: str
        integer i, nchars
        i = 1
        do
            if (s(i) == c_null_char) exit
            i = i + 1
        end do
        nchars = i - 1  ! Exclude null character from Fortran string
        allocate(character(len=nchars) :: str)
        str = transfer(s(1:nchars), str)
    end function c_to_f_string

    subroutine pstr(s) bind(c,name='pstr')
        use iso_c_binding
        character(kind=c_char,len=1), intent(in) :: s(*)
        character(len=:), allocatable :: str
        integer i, nchars
        write(*,'(a)') c_to_f_string(s)
    end subroutine pstr

    end module cstr


!
!
!射线追踪基本数据结构和辅助函数
!
!
module RayStep
	integer,parameter::wp=kind(1.0)
	
	real(wp),dimension(3)::&
		&xyz,&    !XYZ坐标
		&txyz,&   !切线方向(tx,ty,tz)
		&pxyz,&   ! 
		&e1xyz,&  !  
		&e2xyz,&  !
		&vxyz     !速度场梯度(dvdx,dvdy,dvdz)
		
contains

    function cross(x,y) result(z)
		real(wp) ::x(3),y(3),z(3)
		z(1)=x(2)*y(3)-x(3)*y(2)
		z(2)=x(3)*y(1)-x(1)*y(3)
		z(3)=x(1)*y(2)-x(2)*y(1)
	end function cross	 
	
	function squre(x) result(r)
		real(wp)::x(3),r
		
		r=x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
	end function 
	
	function dot(x,y) result(r)
		r=x(1)*y(1)+x(2)*y(2)+x(3)*y(3);
	end function 
	
	function norm(x) result(r)
		real(wp)::x(3),r
		r=sqrt(squre(x));
	end function 

	subroutine vnorm(x)
		real(wp)::x(3)
		real(wp)::r
		r=norm(x)
		r=1/max(epsilon, r)
		x=r*x
	end subroutine  	
	
	subroutine swap(x,y)
		real(wp)::x(3),y(3)
		real(wp)::t
		
		t=x(1)
		x(1)=y(1)
		y(1)=t
		
		t=x(2)
		x(2)=y(2)
		y(2)=t
		
		t=x(3)
		x(3)=y(3)
		y(3)=t
		
	end subroutine 
	
    end module RayStep
    
    

    !
    !
    !  MAIN FUNCTION
    !
    
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
    
 