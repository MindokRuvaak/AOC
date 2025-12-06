module filereader
   implicit none
   
   private

   public readMatrixInt, readStringArray, lengthOfFile
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
         READ (io,* , END=99)
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



end module filereader

