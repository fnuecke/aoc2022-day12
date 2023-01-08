module day12
   implicit none

   private
   public t_cell_index, read_heightmap, find_all_valid_starts, find_shortest_path, find_shortest_path2

   type :: t_cell_index
      integer :: x, y
   end type

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
      character(len=1), intent(in) :: heightmap(:, :)
      character, intent(in) :: symbol
      type(t_cell_index), intent(out) :: index

      integer :: x, y

      outer: do x = 1, size(heightmap, 1)
         do y = 1, size(heightmap, 2)
            if (heightmap(x, y) == symbol) then
               index%x = x
               index%y = y
               return
            end if
         end do
      end do outer

      index%x = 0
      index%y = 0
   end subroutine find_first

   subroutine read_heightmap(filename, heightmap, start, goal)
      implicit none

      character(len=*), intent(in) :: filename
      character(len=1), allocatable, intent(out) :: heightmap(:, :)
      type(t_cell_index), intent(out) :: start, goal

      character(len=1), allocatable :: contents(:)
      integer :: index, x = 1, y = 1, heightmap_dimensions(2)

      call read_file(filename, contents)
      call get_heightmap_size(contents, heightmap_dimensions)

      allocate (heightmap(heightmap_dimensions(1), heightmap_dimensions(2)))
      do index = 1, size(contents)
         if (contents(index) == NEW_LINE('(A)')) then
            y = y + 1
            x = 1
         else
            heightmap(x, y) = contents(index)
            x = x + 1
         end if
      end do

      call find_first(heightmap, "S", start)
      call find_first(heightmap, "E", goal)

      heightmap(start%x, start%y) = "a"
      heightmap(goal%x, goal%y) = "z"
   end subroutine read_heightmap

   subroutine find_all_valid_starts(heightmap, starts)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), allocatable, intent(out) :: starts(:)

      type(t_cell_index) :: start
      integer :: x, y

      allocate(starts(0))
      do x = 1, size(heightmap, 1)
         do y = 1, size(heightmap, 2)
            if (heightmap(x, y) == "a") then
               start%x = x
               start%y = y
               starts = [starts, start]
            end if
         end do
      end do
   end subroutine find_all_valid_starts

   function distance(a, b) result(d)
      type(t_cell_index), intent(in) :: a, b
      real :: d

      real :: d_x, d_y

      d_x = a%x - b%x
      d_y = a%y - b%y
      d = sqrt(d_x*d_x + d_y*d_y)
   end function distance

   subroutine shift_items(open_set, open_set_size, f_scores, from)
      type(t_cell_index), intent(inout) :: open_set(:)
      real, intent(inout) :: f_scores(:)
      integer, intent(in) :: open_set_size, from

      integer :: index

      do index = open_set_size, from + 1, -1
         open_set(index) = open_set(index - 1)
         f_scores(index) = f_scores(index - 1)
      end do
   end subroutine shift_items

   subroutine sorted_insert(open_set, open_set_size, f_scores, value, value_f_score)
      type(t_cell_index), intent(inout) :: open_set(:)
      integer, intent(inout) :: open_set_size
      real, intent(inout) :: f_scores(:)
      type(t_cell_index), intent(in) :: value
      real, intent(in) :: value_f_score

      integer :: index

      type(t_cell_index) :: current

      do index = 1, open_set_size
         current = open_set(index)
         if (f_scores(index) < value_f_score) then
            open_set_size = open_set_size + 1
            call shift_items(open_set, open_set_size, f_scores, index)
            open_set(index) = value
            f_scores(index) = value_f_score
            return
         end if
      end do

      open_set_size = open_set_size + 1
      open_set(open_set_size) = value
   end subroutine sorted_insert

   function is_in_bounds(heightmap, position) result(output)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), intent(in) :: position
      logical :: output

      output = position%x >= 1 .and. position%x <= size(heightmap, 1) .and. position%y >= 1 .and. position%y <= size(heightmap, 2)
   end function is_in_bounds

   function may_move(heightmap, from, to) result(output)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), intent(in) :: from, to
      logical :: output

      integer :: height_from, height_to

      height_from = ICHAR(heightmap(from%x, from%y))
      height_to = ICHAR(heightmap(to%x, to%y))

      output = height_to <= height_from + 1
   end function may_move

   subroutine find_valid_neighbors(heightmap, position, neighbors, neighbor_count)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), intent(out) :: position, neighbors(4)
      integer, intent(out) :: neighbor_count

      type(t_cell_index) :: directions(4), neighbor
      integer :: index

      directions(:)%x = 0
      directions(:)%y = 0
      directions(1)%x = 1
      directions(2)%x = -1
      directions(3)%y = 1
      directions(4)%y = -1

      neighbor_count = 0
      do index = 1, 4
         neighbor%x = position%x + directions(index)%x
         neighbor%y = position%y + directions(index)%y
         if (is_in_bounds(heightmap, neighbor)) then
            if (may_move(heightmap, position, neighbor)) then
               neighbor_count = neighbor_count + 1
               neighbors(neighbor_count) = neighbor
            end if
         end if
      end do
   end subroutine find_valid_neighbors

   function not_in_open_set(open_set, open_set_size, value) result(output)
      type(t_cell_index), intent(in) :: open_set(:), value
      integer, intent(in) :: open_set_size
      logical :: output

      integer :: index
      type(t_cell_index) :: current

      do index = 1, open_set_size
         current = open_set(index)
         if (current%x == value%x .and. current%y == value%y) then
            output = .false.
            return
         end if
      end do

      output = .true.
   end function not_in_open_set

   subroutine find_shortest_path(heightmap, start, goal, length)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), intent(in) :: start, goal
      integer, intent(out) :: length

      type(t_cell_index) :: open_set(size(heightmap, 1)*size(heightmap, 2)), current, neighbor, neighbors(4)
      integer :: open_set_size
      integer :: g_scores(size(heightmap, 1), size(heightmap, 2))
      real :: f_scores(size(open_set))

      integer :: neighbor_count, index, g_score

      open_set(1) = start
      open_set_size = 1

      g_scores(:, :) = -1
      g_scores(start%x, start%y) = 0

      f_scores(1) = distance(start, goal)

      do while (open_set_size > 0)
         current = open_set(open_set_size)
         open_set_size = open_set_size - 1

         if (current%x == goal%x .and. current%y == goal%y) then
            length = g_scores(goal%x, goal%y)
            return
         end if

         call find_valid_neighbors(heightmap, current, neighbors, neighbor_count)

         do index = 1, neighbor_count
            neighbor = neighbors(index)
            g_score = g_scores(current%x, current%y) + 1
            if (g_scores(neighbor%x, neighbor%y) == -1 .or. g_score < g_scores(neighbor%x, neighbor%y)) then
               g_scores(neighbor%x, neighbor%y) = g_score
               if (not_in_open_set(open_set, open_set_size, neighbor)) then
                  call sorted_insert(open_set, open_set_size, f_scores, neighbor, g_score + distance(neighbor, goal))
               end if
            end if
         end do
      end do

      length = -1
   end subroutine find_shortest_path

   subroutine find_shortest_path2(heightmap, starts, goal, length)
      character(len=1), intent(in) :: heightmap(:, :)
      type(t_cell_index), intent(in) :: starts(:), goal
      integer, intent(out) :: length

      integer :: index, tmp_length, best_length
      
      best_length = -1
      do index = 1, size(starts)
         call find_shortest_path(heightmap, starts(index), goal, tmp_length)
         if (best_length == -1 .or. (tmp_length >= 0 .and. tmp_length < best_length)) then
            best_length = tmp_length
         end if
      end do

      length = best_length
   end subroutine find_shortest_path2
end module day12

program aoc2022
   use day12
   implicit none
   character(len=1), allocatable :: heightmap(:, :)
   type(t_cell_index) :: start, goal
   type(t_cell_index), allocatable :: starts(:)
   integer :: length

   call read_heightmap("input.txt", heightmap, start, goal)
   call find_shortest_path(heightmap, start, goal, length)
   print *, "part 1 path length: ", length

   call find_all_valid_starts(heightmap, starts)
   call find_shortest_path2(heightmap, starts, goal, length)
   print *, "part 2 path length: ", length
end program aoc2022
