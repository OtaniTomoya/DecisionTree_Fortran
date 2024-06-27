module decision_tree_io
    use decision_tree_types
    implicit none
contains

    subroutine readCSV(filename, dataset)
        implicit none
        character(len=*), intent(in) :: filename
        type(data_set), intent(out) :: dataset
        integer :: i, j, ios, ncols, nrows, start, endpos
        character(len=8000) :: line
        integer :: unit = 10
        integer, allocatable :: data(:,:)

        nrows = NUM_SAMPLES
        ncols = NUM_FEATURES + 1 ! ラベルを含める
        allocate(data(nrows, ncols))

        open(unit=unit, file=filename, status='old', action='read', iostat=ios)

        do i = 1, nrows
            read(unit, '(A)', iostat=ios) line
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

        allocate(dataset%data(nrows, ncols-1))
        dataset%data = data(:, 1:ncols-1)
        dataset%labels = data(:, ncols)
        dataset%num_samples = nrows
    end subroutine readCSV

    subroutine trainTestSplit(dataset, train_data, test_data, train_ratio)
        type(data_set), intent(in) :: dataset
        type(data_set), intent(out) :: train_data, test_data
        real, intent(in) :: train_ratio
        integer :: train_samples, test_samples

        train_samples = int(dataset%num_samples * train_ratio)
        test_samples = dataset%num_samples - train_samples

        allocate(train_data%data(train_samples, NUM_FEATURES))
        allocate(train_data%labels(train_samples))
        train_data%num_samples = train_samples
        train_data%data = dataset%data(1:train_samples, :)
        train_data%labels = dataset%labels(1:train_samples)

        allocate(test_data%data(test_samples, NUM_FEATURES))
        allocate(test_data%labels(test_samples))
        test_data%num_samples = test_samples
        test_data%data = dataset%data(train_samples+1:dataset%num_samples, :)
        test_data%labels = dataset%labels(train_samples+1:dataset%num_samples)
    end subroutine trainTestSplit

end module decision_tree_io
