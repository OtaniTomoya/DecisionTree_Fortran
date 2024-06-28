program test_data_structures
    implicit none

    type :: data_set
        integer, allocatable :: data(:,:)
        integer, allocatable :: labels(:)
        integer :: num_samples
    end type data_set

    integer, parameter :: n = 1000
    integer :: i, j
    real :: start_time, end_time, structure_time, array_time
    type(data_set) :: ds
    integer, allocatable :: data(:,:)
    integer, allocatable :: labels(:)

    ! Allocate memory
    allocate(ds%data(n, n))
    allocate(ds%labels(n))
    allocate(data(n, n))
    allocate(labels(n))
    structure_time = 0
    array_time = 0
    ! Measure time for data_set
    do j = 1, 100
        call cpu_time(start_time)
        do i = 1, 10000
            call process_with_data_set(ds, n)
        end do
        call cpu_time(end_time)
        structure_time = structure_time + end_time - start_time

        ! Measure time for arrays
        call cpu_time(start_time)
        do i = 1, 10000
            call process_with_arrays(data, labels, n)
        end do
        call cpu_time(end_time)
        array_time = array_time + end_time - start_time
    end do
    print *, "Structure time: ", structure_time*0.01, " seconds", "Array time: ", array_time*0.01, " seconds"
    ! Structure time:    5.00492096      secondsArray time:    1.55737424      seconds
contains

    subroutine process_with_data_set(ds, n)
        type(data_set), intent(inout) :: ds
        integer, intent(in) :: n
        integer :: i, j

        do i = 1, n
            ds%labels(i) = i
            do j = 1, n
                ds%data(i, j) = i * j
            end do
        end do
    end subroutine process_with_data_set

    subroutine process_with_arrays(data, labels, n)
        integer, allocatable, intent(inout) :: data(:,:)
        integer, allocatable, intent(inout) :: labels(:)
        integer, intent(in) :: n
        integer :: i, j

        do i = 1, n
            labels(i) = i
            do j = 1, n
                data(i, j) = i * j
            end do
        end do
    end subroutine process_with_arrays

end program test_data_structures
