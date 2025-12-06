module filereader
   implicit none
   
   private

   public lengthOfFile, readMatrixInt, readStringArray, readCSStringArray
contains

   subroutine lengthOfFile(nlines, filePath, lfp)
      integer :: io
      integer, intent(in) :: lfp
      integer, intent(out) :: nlines
      character(len=lfp), intent(in) :: filePath
      nlines = 0

      open(newunit=io, file=filePath, status="old", action="read")
      ! counts number of lines in file
      DO
         READ (io, *, END=99)
         nlines = nlines + 1
      END DO
99    CLOSE (io)
   end subroutine lengthOfFile


   subroutine readMatrixInt(resMatrix, m, n, filePath, lfp)
      integer :: io
      integer, intent(in) :: m, n, lfp
      character(len=lfp), intent(in) :: filePath
      integer, intent(inout) :: resMatrix(m, n)

      open(newunit=io, file=filePath, status="old", action="read")
      read(io, *) resMatrix
      close(io)
   end subroutine readMatrixInt


   subroutine readStringArray(resVector, n, filePath, lfp)
      integer :: io
      integer, intent(in) :: n, lfp
      character(len=*), intent(inout) :: resVector(n)
      character(len=lfp), intent(in) :: filePath

      open(newunit=io, file=filePath, status="old", action="read")
      read(io, *) resVector
      close(io)
   end subroutine readStringArray


   subroutine readCSStringArray(resultArray, fromFile, lfromfp, seperator)
      integer :: fileSize, io, c, i
      integer, intent(in) :: lfromfp
      character(len=:), allocatable       :: fileContent
      ! character(len=ltofp),   intent(in)  :: toFile
      character(len=lfromfp), intent(in)  :: fromFile
      character, intent(in)               :: seperator
      ! character                           :: parseChar
      character(len=:), allocatable, intent(inout) :: resultArray(:)
      c=0

      ! print *, "reading size of file"//' '//fromFile
      open(newunit=io, file=fromFile, status="old", action="read")
      inquire(unit=io, size=fileSize)
      close(io)
      ! print *, "size = ", fileSize
      allocate(character(len=fileSize) :: fileContent)
      ! print *, "reading contents..."
      open(newunit=io, file=fromFile, status="old", action="read", form="unformatted", access="stream")
      read(io) fileContent
      close(io)
      print *, fileContent


      do concurrent (i = 1:fileSize)
         read(fileContent,'(a)', end=99)
         if (fileContent(i:i) == seperator) then
            c=c+1
            if (fileContent((i+1):(i+1)) == new_line('a')) then
               fileContent(i:(i+1)) = new_line('a')
            else 
               fileContent(i:i) = new_line('a')
            end if
         end if
99    end do

      ! print *, fileContent
      allocate(character(25) :: resultArray(c))
      ! print *, "storing in array of size: ", c
      ! THIS IGNORES EMPTY ROWS ! - pogging
      read(fileContent,*) resultArray
      ! print *, resultArray

   end subroutine readCSStringArray



end module filereader

