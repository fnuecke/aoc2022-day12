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

      open (newunit=file, file=filename, status="old", action="read", form="unformatted", access="stream")
      inquire (unit=file, size=filesize)
      allocate (contents(filesize))
      read (file) contents
      close (file)
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
         else
            line_length = line_length + 1
         end if
      end do

      dimensions(1) = max_line_length
      dimensions(2) = lines
   end subroutine get_heightmap_size

   subroutine find_first(heightmap, symbol, index)
      character(len=1), allocatable, intent(in) :: heightmap(:, :)
      character, intent(in) :: symbol
      integer, intent(out) :: index(2)

      integer :: x, y

      outer: do x = 1, size(heightmap, 1)
         do y = 1, size(heightmap, 2)
            if (heightmap(x, y) == symbol) then
               index(1) = x
               index(2) = y
               return
            end if
         end do
      end do outer

      index(1) = 0
      index(2) = 0
   end subroutine find_first

   subroutine read_heightmap(filename, heightmap, start, goal)
      implicit none

      character(len=*), intent(in) :: filename
      character(len=1), allocatable, intent(out) :: heightmap(:, :)
      integer, intent(out) :: start(2)
      integer, intent(out) :: goal(2)

      character(len=1), allocatable :: contents(:)
      integer :: index, x = 1, y = 1, heightmap_dimensions(2)

      call read_file(filename, contents)
      call get_heightmap_size(contents, heightmap_dimensions)

      allocate(heightmap(heightmap_dimensions(1), heightmap_dimensions(2)))
      do index = 1, size(contents)
         if (contents(index) == NEW_LINE('(A)')) then
            y = y + 1
            x = 0
         else
            heightmap(x, y) = contents(index)
            x = x + 1
         end if
      end do

      call find_first(heightmap, "S", start)
      call find_first(heightmap, "E", goal)
   end subroutine read_heightmap
end module day12

program aoc2022
   use day12
   implicit none
   character(len=1), allocatable :: heightmap(:, :)
   integer :: start(2), goal(2)

   call read_heightmap("test_input.txt", heightmap, start, goal)

   print *, "size:", size(heightmap, 1), size(heightmap, 2)
   print *, "start: ", start
   print *, "goal: ", goal
end program aoc2022
