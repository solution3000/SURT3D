    !
    !  C 和 Fortran的互相调用
    !
    
    
    
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

