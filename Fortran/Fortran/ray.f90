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
		
		t=x(1);	x(1)=y(1);	y(1)=t
		
		t=x(2);	x(2)=y(2);	y(2)=t
		
		t=x(3);	x(3)=y(3);	y(3)=t
		
	end subroutine 
	
end module RayStep