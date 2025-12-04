! read column of values, 
! each value is of type (L|R)n+
! L -> move left, R -> move right
! n -> number of clicks
! count number of times 
program day1task
    use filereader
    integer :: n, acc, totZeros, change, nTurns
    character(len=5), allocatable :: rawVec(:)
    character(len=1), allocatable :: charVector(:)
    character(len=:), allocatable :: fp
    integer, allocatable :: numVector(:)
    ! fp = "day1Test.dat"
    fp = "day1.dat"
    acc = 50 ! starts on 50
    totZeros = 0

    call lengthOfFile(nLines=n, filePath=fp, lfp=len(fp))
    allocate(rawVec(n))
    allocate(charVector(n))
    allocate(numVector(n))
    call readVectorString(resVector=rawVec, n=n, filePath=fp, lfp=len(fp))

    call firstChars(charVector, rawVec, n)
    call getNums(numVector, rawVec, n)

    do j = 1, n
        change = readInstruction(charVector(j), numVector(j))
        ! print *, change
        nTurns = abs(change)/100
        totZeros = totZeros + nTurns
        change = mod(change, 100)
        ! print *, change, ": ", nTurns 
        if (acc /= 0) then
            if (acc + change < 0) then
                totZeros = totZeros + 1
            else if (acc + change > 100) then
                totZeros = totZeros + 1
            end if
        end if
        acc = mod(100 + acc + change, 100)
        if (acc == 0) then
            totZeros = totZeros + 1
        end if
        ! print *, rawVec(j), " => ", acc, " => ", totZeros
    end do

    print *, totZeros
    deallocate(rawVec)
    deallocate(charVector)
    deallocate(numVector)
contains 
    function readInstruction(dir, amount) result(delta)
        implicit none
        integer :: delta
        integer, intent(in) :: amount
        character(len=1), intent(in) :: dir
        if (dir == 'L') then
            delta = - amount
        else if (dir == 'R') then
            delta = amount
        else 
            print *, "something went wrong"
        end if

    end function readInstruction


    subroutine firstChars(output, input, nlen)
        integer, intent(in) :: nlen
        character(len=*), intent(in) :: input(nlen)
        character(len=1), intent(out) :: output(nlen)
        do concurrent (i = 1:nlen)
            output(i) = input(i)(1:1)
        end do
    end subroutine firstChars

    subroutine getNums(output, input, nlen)
        integer, intent(in) :: nlen
        character(len=*), intent(in) :: input(nlen)
        integer, intent(out) :: output(nlen)
        do concurrent (i = 1:nlen)
            read (input(i)(2:), '(i5)') output(i)
        end do
    end subroutine getNums

end program day1task