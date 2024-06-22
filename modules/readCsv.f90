module read_csv
    implicit none
    integer, parameter :: NUM_FEATURES = 784
    integer, parameter :: NUM_SAMPLES = 70

contains

    subroutine readCSV(filename, dataSet)
        implicit none
        character(len=*), intent(in) :: filename
        integer :: i, j, ios, start, endpos
        integer, parameter :: ncols = NUM_FEATURES + 1
        integer :: dataSet(NUM_SAMPLES, ncols)
        character(len=8000) :: line
        integer :: unit = 10
        open(unit=unit, file=filename, status='old', action='read', iostat=ios)
        do i = 1, NUM_SAMPLES
            read(unit, '(A)', iostat=ios) line
            start = 1
            do j = 1, ncols
                endpos = index(line(start:), ',')
                if (endpos == 0) then
                    endpos = len_trim(line(start:)) + 1
                end if
                read(line(start:start+endpos-2), '(I10)', iostat=ios) dataSet(i, j)
                start = start + endpos
            end do
        end do
        close(unit)
    end subroutine readCSV
    subroutine loadDataset(filename)
        implicit none
        integer, parameter :: ncols = NUM_FEATURES+1
        integer :: samples(NUM_SAMPLES, ncols)
        integer :: labels(NUM_SAMPLES)
        character(len=100) :: filename
        integer :: i, j

        call readCSV(filename, samples)

        do i=1, ncols
            do j=1, NUM_SAMPLES

            end do
        end do
    end subroutine loadDataset
end module

program test_readCSV
    use read_csv
    implicit none
    integer, parameter :: ncols = NUM_FEATURES+1
    integer :: samples(NUM_SAMPLES, ncols)
    integer :: labels(NUM_SAMPLES)
    character(len=100) :: filename
    integer :: i, j
    filename = '/Users/tomoya/Code/GA/mnist.csv'
    call readCSV(filename, samples)
    do i=1, NUM_SAMPLES
        labels(i) = samples(i, ncols)
        print *, labels(i)
    end do
end program test_readCSV
