program LoadDiffSum
   use sorting
   integer :: io, i, f
   integer, dimension(2000) :: a
   integer, dimension(1000) :: b, c, e
   open(newunit=io, file="day1.dat", status="old", action="read")
   read(io, *) a

   ! read input seperated into two vectors
   do i = 1,1000
      b(i) = a(2*i-1)
      c(i) = a(2*i)
   end do

   !comments

   ! sort the two vectors
   call quicksort(b,1,1000)
   call quicksort(c,1,1000)

   do i = 1,1000
      e(i) = abs(b(i) - c(i))
   end do
   f = sum(e)
   ! print *, b
   write(*,*) (b(i), c(i), e(i), new_line("a"), i = 1, 1000)
   print*, f
end program LoadDiffSum
