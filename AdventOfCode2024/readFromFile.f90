module filereader
   implicit none
   
   private

   public readMatrixInt
contains

   subroutine readMatrixInt(m, n, resMatrix, l, filePath)
      integer :: io
      integer, intent(in) :: m, n, l
      integer, intent(inout) :: resMatrix(m, n)
      character(len=l), intent(in) ::  filePath
      open(newunit=io, file=filePath, status="old", action="read")
      read(io, *) resMatrix
      close(io)
   end subroutine readMatrixInt

end module filereader

