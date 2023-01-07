module day12
    implicit none

    private
    public read_heightmap

contains
    subroutine read_file(filename, contents)
        implicit none
        
        character(len=*), intent(in) :: filename
        character(len=1), allocatable, intent(out) :: contents(:)

        integer :: file
        integer :: filesize

        open(newunit=file, file=filename, status="old", action="read", form="unformatted", access="stream")
        inquire(unit=file, size=filesize)
        allocate(contents(filesize))
        read(file) contents
        close(file)
    end subroutine read_file

    subroutine get_heightmap_size(contents, dimensions)
        implicit none

        character, intent(in) :: contents(:)
        integer, intent(out) :: dimensions(2)

        integer :: index, lines = 0, line_length = 0, max_line_length = 0
    
        do index = 1, size(contents)
            if (contents(index) == NEW_LINE('(A)')) then
                lines = lines + 1
                max_line_length = max(max_line_length, line_length)
                line_length = 0
            endif
            line_length = line_length + 1
        end do

        dimensions(1) = max_line_length
        dimensions(2) = lines
    end subroutine get_heightmap_size

    subroutine read_heightmap(filename, heightmap)
        implicit none

        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: heightmap(:,:)

        character(len=1), allocatable :: contents(:)
        integer :: heightmap_dimensions(2)

        call read_file(filename, contents)
        call get_heightmap_size(contents, heightmap_dimensions)

        allocate(heightmap(heightmap_dimensions(1), heightmap_dimensions(2)))
    end subroutine read_heightmap
end module day12

program aoc2022
    use day12
    implicit none
    integer, allocatable :: heightmap(:,:)

    write (*,*) "test"
    call read_heightmap("test_input.txt", heightmap)

    print *, "size:", size(heightmap, 1), size(heightmap, 2)
end program aoc2022
