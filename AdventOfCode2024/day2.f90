
program AOCday2
   ! ragged array definition by High Performance Mark on a stack overflow thread at
   ! https://stackoverflow.com/questions/18316592/multidimensional-array-with-different-lengths
   type :: vector
      integer, dimension(:), allocatable :: elements
   end type vector

   type :: ragged_array
      type(vector), dimension(:), allocatable :: vectors
   end type ragged_array

   ! use filereader
   ! implicit none
   integer :: i, n, s
   integer :: j, k
   logical, allocatable :: boolarr(:)
   type(ragged_array) :: raggedMatrix
   ! logical :: limitedDescending, limitedAscending
   character(:), allocatable :: fp
   fp = "day2test.dat"
   fp = trim(fp)
   n = 0

   call readFileAsRaggedArray(raggedMatrix, len(fp), fp, n)
   ! print *, size(raggedMatrix%vectors)
   print*, n
   do j = 1,size(raggedMatrix%vectors)
      print*, size(raggedMatrix%vectors(i)%elements)
      do k = 1,size(raggedMatrix%vectors(i)%elements)
         print*, raggedMatrix%vectors(i)%elements
      end do
   end do
!    call readMatrixInt(10, 1000, mat, len(fp), fp)
   allocate(boolarr(size(raggedMatrix%vectors)))
   do i=1,size(raggedMatrix%vectors)
      s = size(raggedMatrix%vectors(i)%elements)
      ! all decreasing or all increrasing
      boolarr(i) = limitedDescending(raggedMatrix%vectors(i)%elements, s, 1, 3) .or. &
      & limitedAscending(raggedMatrix%vectors(i)%elements, s, 1, 3)
   end do
   ! write(*,*) (testmat(:,i), new_line("a"),i=1,6)
   write (*,*) boolarr

contains

   subroutine readFileAsRaggedArray(resArray, l, filePath, nlines)
      integer :: io, ind, jnd
      integer, intent(in) :: l
      integer, intent(out) :: nlines
      type(ragged_array), intent(inout) :: resArray
      character(len=l), intent(in) ::  filePath
      character(len=20), allocatable, dimension(:) :: temp
      open(newunit=io, file=filePath, status="old", action="read")
      ! counts number of lines in file
      DO
         READ (io,*, END=10)
         nlines = nlines + 1
      END DO
10    CLOSE (io)
      ! print*, nlines

      allocate(resArray%vectors(nlines))
      allocate(temp(nlines))

      ! print*, size(resArray%vectors)
      ! print*, size(temp)
      ! rewind(io)
      open(newunit=io, file=filePath, status="old", action="read")
      DO ind =1,nlines
         READ (io, '(A)') temp(ind)
         ! print*, len(trim(temp(ind))), trim(temp(ind))
         do jnd =1:len(temp(ind))
            
         end do
      END DO


      close(io)
   end subroutine readFileAsRaggedArray

   ! ll, ul both non-negative and ll < ul !
   ! PURE
   function limitedDescending(elements, n, ll, ul) result(boolean)
      implicit none
      integer, intent(in) :: ll, ul, n
      integer :: ind
      integer, intent(in) :: elements(n)
      logical :: boolean
      integer :: diff_arr(size(elements)-1)
      ! checks if vector  (vec) is in decending order with the decrease limited to values between
      ! a lower limit (ll) and an upper limit (ul)
      do ind = 1,(size(elements)-1)
         ! decentding order: highest to lowest, these should always be positive
         diff_arr(ind) = elements(ind) - elements(ind+1)
         print *, diff_arr(ind)
      end do
      boolean = all(ll <= diff_arr .and. diff_arr <= ul)
   end function limitedDescending

   ! ll, ul both non-negative and ll < ul !
   ! PURE
   function limitedAscending(elements, n, ll, ul) result(boolean)
      implicit none
      integer, intent(in) :: ll, ul, n
      integer :: ind
      integer, intent(in) :: elements(n)
      logical :: boolean
      integer :: diff_arr(size(elements)-1)
      ! checks if vector  (vec) is in ascending order with the decrease limited to values between
      ! a lower limit (ll) and an upper limit (ul)
      do ind = 1,(size(elements)-1)
         ! ascending order: lowest to highest, these should always be positive
         diff_arr(ind) = elements(ind+1) - elements(ind)
         print *, diff_arr(ind)
      end do
      boolean = all(ll <= diff_arr .and. diff_arr <= ul)
   end function limitedAscending
end program AOCday2
