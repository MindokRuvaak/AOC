program day2
    use filereader
    integer :: lfp
    character(len=:), allocatable   :: fp
    character(len=:), allocatable   :: dataArray(:)
    integer, dimension(2)           :: range
    fp = "day2test.dat"
    lfp = len(fp)
    
    
    call readCSStringArray(resultArray=dataArray, fromFile=fp, lfromfp=lfp, seperator=',')
    
    do i=1, size(dataArray)
        print *, dataArray(i)
        range = findRange(rangeString = dataArray(i), strLen = len(dataArray(i)), seperator='-')
        print *, range
        call examineRange(from = range(1), to = range(2))
    end do


contains

    function findRange(rangeString, strLen, seperator) result(limits)
        integer, dimension(2) :: limits
        integer, intent(in)                 :: strLen
        character(len=strLen), intent(in)   :: rangeString
        character, intent(in)               :: seperator

        do i = 1, strLen
            if (rangeString(i:i) == seperator) then
                read(rangeString(:(i-1))), limits(1)
                read(rangeString((i+1):)), limits(2)
                exit
            end if
        end do
    end function



    subroutine examineRange(from, to)
        integer, intent(in) :: from, to
        integer :: sum
        sum = 0
        do concurrent (i = from:to)
            if (isWrongId) then
                sum = sum + i
            end if
        end do
    end subroutine

    function isWrongId(idNum) result(bool)
        integer idNum, digitCount, halfNum
        logical bool
        character(len=:), allocatable :: first, second
        digitCount = numDigits(idNum)
        halfNum = digitCount/2


        if (mod(digitCount, 2) == 0 ) then
            
            bool = .true. ! temporary
        else
            bool = .false. ! any number with odd number of digits will be a valid id
        end if
    end function


    function numDigits(idNum) result(count)
        integer idNum, temp, count
        count = 0
        temp = idNum
        do while (temp /= 0)
            temp = temp / 10
            count = count + 1
        end do
        end function

end program day2