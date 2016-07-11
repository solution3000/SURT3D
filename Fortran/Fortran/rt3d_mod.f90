
!RAYTRACE3D requires an input file (drt.inp) that the program can 
!read the input parameters from the file. The following lists 
!the parameters and their default values.
!xs=0.0		x-coordinate of single source location
!ys=0.0		y-coordinate of single source location
!zs=0.0		z-coordinate of single source location
!vx_file=	name of input velocity file
!nz_vel=1	number of z samples (1st dimension) in velocity	
!z0_vel=0.0	first z sample of velocity	
!dz_vel=1.0	z sampling interval of velocity	
!nx_vel=1	number of x samples (2nd dimension) in velocity	
!x0_vel=0.0	first x sample of velocity	
!dx_vel=1.0	x sampling interval of velocity	
!ny_vel=1	number of y samples (3rd dimension) in velocity	
!y0_vel=0.0	first y sample of velocity	
!dy_vel=1.0	y sampling interval of velocity	
!v0=2000.0	constant background velocity
!vx=0.0		x coefficient of linear velocity
!vy=0.0		y coefficient of linear velocity
!vz=0.0		z coefficient of linear velocity
!na=10		number of initial samples of polar take-off angle  
!a0=0.0		first sample of polar take-off angle 
!a1=90.0		last sample of polar take-off angle 
!nb=10		number of initial samples of azimuthal take-off angle  
!b0=0.0		first sample of azimuthal take-off angle 
!b1=360.0	last sample of azimuthal take-off angle
!tim_type=DRAY	forward modeling type, (this version only allows DRAY)
!		=EIK	eikonal solver
!		=PRAY	paraxial raytracing
!		=DRAY	dynamic raytracing
!t_scale=10	number of micro-steps in each macro-step of ray tracing
!inter_type=1	type of arrivals stored in output grids
!		=1	most energetic
!		=2	first arrival
!		=3	shortest raypath
!		=4	smallest maximum velocity
!		=5	longest travel time
!		=6	longest raypath
!num_add=1	number of macro-steps that adding rays function is called
!t0_ray=0.0005	initial ray tracing time (in second)
!t1_ray=10.0	maximum ray tracing time (in second)
!dt_ray=0.02	macro ray tracing step in time (in second)
!dr_ray=250.0	distance limit between rays for new rays to be inserted
!m_ray=10001	maximum number of rays to be generated
!maxangle=180.0	maximum take-off angle to stop adding rays and tracing rays
!mx_tab=1	number of x samples in output grids
!dx_tab=1.0	x sampling interval of output grids
!my_tab=1	number of y samples in output grids
!dy_tab=1.0	y sampling interval of output grids
!mz_tab=1	number of z samples in output grids
!dz_tab=1.0	z sampling interval of output grids
!outlog_file=	name of output log file that contains reading parameter values
!outt_file=	name of output travel time table
!outamp_file=	name of output amplitude table
!outpha_file=	name of output phase shift index table
!outa1_file=	name of output polar angle table
!outb1_file=	name of output azimuth angle table
!outdet21_file=	name of output table for Beylkin determinant components
!outdet22_file=	name of output table for Beylkin determinant components
!outdet23_file=	name of output table for Beylkin determinant components
!outdet31_file=	name of output table for Beylkin determinant components
!outdet32_file=	name of output table for Beylkin determinant components
!outdet33_file=	name of output table for Beylkin determinant components

module rt3d_mod

	integer,parameter::wp=kind(1.0)
	!m_ray=10001	maximum number of rays to be generated
	integer,parameter::m_ray=30001
	
	real(wp) ::s_xyz0(3) !炮点位置
	real(wp) ::v_xyz0(3) !速度场原点坐标
	real(wp) ::t_xyz0(3) !旅行时原点坐标
	
	integer ::nxv,nyv,nzv  !速度场网格
	integer ::nxt,nyt,nzt  !旅行时网格
	
	real(wp) :: dxv, dyv, dzv !速度场网格长度
	real(wp) :: dxt, dyt, dzt !旅行时网格长度
	
	!初始射线角度信息
	integer::na,nb   !na=91, nb=120
	real(wp) ::a0,a1 !a0=0.0, a1=90.0 polar take-off angle
	real(wp) ::b0,b1 !b0=0.0, b1=360.0 azimuthal take-off angle
	
	!射线控制参数
	real(wp) :: maxangle !maxangle=120.00
	real(wp) :: t0,t1    !t0=0.00050, t1=10.00秒
	real(wp) :: dt !dt=0.02, time积分步长
	real(wp) :: dr_limit !dr=250.00, distance limit between rays for new rays to be inserted
	integer::dt_steps !dt_steps=10, number of micro-steps in each macro-step of ray tracing
	
	!inter_type=1	type of arrivals stored in output grids
	!		=1	most energetic
	!		=2	first arrival
	!		=3	shortest raypath
	!		=4	smallest maximum velocity
	!		=5	longest travel time
	!		=6	longest raypath	
	integer :: inter_type ! 1~6
	
	integer, parameter:: num_add=3 !每次增加的射线数目
	
	!================================
	!  输入输出文件信息
	!=================================
	
	!速度文件
	character*(*) :: vx_file

	!outt_file=	name of output travel time table
	!outamp_file=	name of output amplitude table
	!outpha_file=	name of output phase shift index table
	!outa1_file=	name of output polar angle table
	!outb1_file=	name of output azimuth angle table
	!outdet21_file=	name of output table for Beylkin determinant components
	!outdet22_file=	name of output table for Beylkin determinant components
	!outdet23_file=	name of output table for Beylkin determinant components
	!outdet31_file=	name of output table for Beylkin determinant components
	!outdet32_file=	name of output table for Beylkin determinant components
	!outdet33_file=	name of output table for Beylkin determinant components
	
	!output
	character*(*) ::outt_file
	character*(*) ::outamp_file
	character*(*) ::outpha_file
	character*(*) ::outdet21_file
	character*(*) ::outdet22_file
	character*(*) ::outdet23_file
	character*(*) ::outdet31_file
	character*(*) ::outdet32_file
	character*(*) ::outdet33_file
	character*(*) ::outa1_file
	character*(*) ::outb1_file
	character*(*) ::outinform

	!---------------------------------------
	!
	!   派生出来的变量
	!
	!----------------------------------------
	
	real(wp),dimension(3,m_ray,2) :: &
		&t_xyz,&  !射线xyz坐标
		&txyz,&   !射线切线方向矢量
		&pxyz,&   !射线轴向方向矢量
		&e1xyz,&  !e1方向向量
		&e2xyz,&  !e2方向向量, e3=cross(e1,e2)
		&vxyz&    !dvdx,dvdy,dvdz
	real(wp),dimension(m_ray)::	&
		&a_ray,&  !a角度
		&b_ray,&  !b角度
		&s_ray    !射线长度
	real(wp), dimension(4,m_ray)::&
		&qq_ray,&
		&hq_ray
	
	integer,parameter::m_tube=30000
	integer ::n_tube

	! n_tube = actual number of ray tubes established
	! ir_tube = integer array of tube pointers established dimensioned 3,m_tube
	! it_tube = integer array of side pointers established dimensioned 3,m_tube
	! is_tube = integer array of side pointers established dimensioned 3,m_tube

	integer,dimension(3,m_tube)::&
		&ir_tube,&
		&is_tube,&
		&it_tube
	integer,dimension(m_tube)::&
		&it_flag
	
	
	
	
end module 