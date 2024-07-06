program makeMnistData
    implicit none
    integer, allocatable :: datas(:,:)
    integer, allocatable :: labels(:)
    integer :: i, j, ios, ncols, nrows, start, endpos
    character(len=8000) :: line
    integer :: unit = 10
    integer, allocatable :: data(:,:)
    integer, parameter :: NUM_FEATURES = 784
    integer, parameter :: DATASET_SIZE = 70000
    integer :: ierr, recl

    nrows = DATASET_SIZE
    ncols = NUM_FEATURES + 1 ! ラベルを含める
    allocate(data(nrows, ncols))

    open(unit=unit, file="mnist.csv", status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening mnist.csv file."
        stop
    end if

    do i = 1, nrows
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        start = 1
        do j = 1, ncols
            endpos = index(line(start:), ',')
            if (endpos == 0) then
                endpos = len_trim(line(start:)) + 1
            end if
            read(line(start:start+endpos-2), '(I3)', iostat=ios) data(i, j)
            start = start + endpos
        end do
    end do
    close(unit)

    allocate(datas(nrows, ncols-1))
    datas = data(:, 1:ncols-1)
    allocate(labels(nrows))
    labels = data(:, ncols)

    open(10, file="dataset.dat", form="unformatted")
    write(10) datas, labels
    close(10)
end program makeMnistData
