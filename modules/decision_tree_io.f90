module decision_tree_io
    use decision_tree_types
    implicit none
contains

    subroutine readCSV(filename, datas, labels)
        implicit none
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: datas(:,:)
        integer, allocatable, intent(out) :: labels(:)
        integer :: i, j, ios, ncols, nrows, start, endpos
        character(len=8000) :: line
        integer :: unit = 10
        integer, allocatable :: data(:,:)

        nrows = DATASET_SIZE
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

        allocate(datas(nrows, ncols-1))
        datas = data(:, 1:ncols-1)
        allocate(labels(nrows))
        labels = data(:, ncols)
    end subroutine readCSV

    subroutine trainTestSplit(dataset, labels, num_samples, &
            train_dataset, train_labels, train_samples, &
            test_dataset, test_labels, test_samples, train_ratio)
        integer, allocatable, intent(out) :: train_dataset(:,:)
        integer, allocatable, intent(out) :: train_labels(:)
        integer, intent(out) :: train_samples
        integer, allocatable, intent(out) :: test_dataset(:,:)
        integer, allocatable, intent(out) :: test_labels(:)
        integer, intent(out) :: test_samples
        integer, allocatable, intent(in) :: dataset(:,:)
        integer, allocatable, intent(in) :: labels(:)
        integer, intent(in) :: num_samples
        real, intent(in) :: train_ratio

        train_samples = int(num_samples * train_ratio)
        test_samples = num_samples - train_samples

        allocate(train_dataset(train_samples, NUM_FEATURES))
        allocate(train_labels(train_samples))
        train_dataset = dataset(1:train_samples, :)
        train_labels = labels(1:train_samples)

        allocate(test_dataset(test_samples, NUM_FEATURES))
        allocate(test_labels(test_samples))
        test_dataset = dataset(train_samples+1:num_samples, :)
        test_labels = labels(train_samples+1:num_samples)
    end subroutine trainTestSplit

end module decision_tree_io