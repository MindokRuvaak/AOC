program LoadDiffSum
   use sorting
   use filereader
   integer :: io, i, f, similarityMult, j, similarrityScoreAccumulator
   integer :: mat(2,1000)
   integer, dimension(2000) :: a
   integer, dimension(1000) :: b, c, e, diff
   open(newunit=io, file="day1.dat", status="old", action="read")
   read(io, *) a
   close(io)

   ! read input seperated into two vectors
   do i = 1,1000
      b(i) = a(2*i-1)
      c(i) = a(2*i)
   end do

   call readMatrixInt(m=2, n=1000, resMatrix=mat, l=8, filePath="day1.dat")

   print*, "Matrix reader test:"
   call quicksort(mat(1,:),1,1000)
   call quicksort(mat(2,:),1,1000)
   ! print*, mat
   ! write(*,*) (mat(1,i), mat(2,i), new_line("a"), i = 1, 1000)

   do i = 1,1000
      diff(i) = abs(mat(1,i) - mat(2,i))
   end do
   print*, sum(diff)

   ! sort the two vectors
   call quicksort(b,1,1000)
   call quicksort(c,1,1000)

   do i = 1,1000
      e(i) = abs(b(i) - c(i))
   end do
   f = sum(e)
   ! print *, b
   ! write(*,*) (b(i), c(i), e(i), new_line("a"), i = 1, 1000)
   print*, "Task1:"
   print*, f


   outer : do i = 1,1000
      similarityMult  = 0
      inner : do j = 1,1000
         if (b(i) == c(j)) then
            similarityMult = similarityMult + 1
         end if
      end do inner
      similarrityScoreAccumulator = similarrityScoreAccumulator + b(i)*similarityMult
   end do outer

   print*, "Task2:"
   print*, similarrityScoreAccumulator

end program LoadDiffSum
