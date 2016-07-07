module util_me

contains

    subroutine util_setr(n,x,x0)
      implicit none
      integer  i,n
      !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
      real     x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
    end subroutine 

   subroutine util_seti(n,x,x0)
      implicit none
      integer  i,n
      !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
      integer  x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
      end subroutine 

      subroutine util_lini(n,x,x0,dx)
      implicit none
      integer  i,n
      !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
      integer  x(1),x0,dx
      do i = 1 , n
        x(i) = x0 + (i - 1 ) * dx
      enddo
      return
      end subroutine 
      
end module util_me