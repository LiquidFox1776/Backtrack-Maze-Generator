' A maze generator that uses a backtrack algorithm
' Devoloped using FreeBASIC and FBIDE
'
'Copyright 2018 LiquidFox1776
'
'Permission is hereby granted, free of charge, to any person obtaining a copy
'of this software and associated documentation files (the "Software"), to deal
'in the Software without restriction, including without limitation the rights
'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
'copies of the Software, and to permit persons to whom the Software is
'furnished to do so, subject to the following conditions:
'
'The above copyright notice and this permission notice shall be included
'in all copies or substantial portions of the Software.
'
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
'OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'
' There are a couple of sleep statements in the GenerateMaze and Backtrack routines
' that have been commented out, you can uncomment them and tweak them to slow down the animation
'
Const NORTH_WALL = 1
Const SOUTH_WALL = 2
CONST WEST_WALL = 4
CONST EAST_WALL = 8

Enum COLORS
    BLACK
    BLUE
    GREEN
    AQUA
    RED
    PURPLE
    YELLOW
    WHITE
    GRAY
    LIGHT_BLUE
    LIGHT_GREEN
    LIGHT_AQUA
    LIGHT_RED
    LIGHT_PURPLE
    LIGHT_YELLOW
    LIGHT_WHITE
end enum

Type PLOC_TYPE ' Point Location
    x as Integer
    y as Integer
End Type

Type SQUARE_TYPE
    x1 as integer ' upper left coord
    y1 as integer ' lower left coord
    x2 as integer ' upper right coord
    y2 as integer ' lower right coord
    fillColor as integer
    borderColor as integer
End Type

Type CELL_TYPE
    borderProperties as uByte ' Format x x x x RIGHT_BLOCKED LEFT_BLOCKED DOWN_BLOCKED UP_BLOCKED
    border as SQUARE_TYPE
    visited as uByte ' if the cell has been visted this is 1 / true
End Type

Type GRID_TYPE
    origin as PLOC_TYPE ' starting graphical location of the first cwll in the grid
    height as Integer ' height of the grid in cells
    width as Integer ' width of the grid in cells
    numberOfCells As Integer ' total number of cells
    numberOfCellsVisited as Integer
    cellSize as Integer ' graphical dimensions of the cell
    currentCell As Integer ' keeps track of the current cell being used
    cellStackIndex as Integer ' index for cellStack
    cells as CELL_TYPE Ptr ' array of cells
    cellStack as Integer Ptr ' keeps track of the move smade
End Type

Declare Function MoveToNextCell(grid As GRID_TYPE, move As String) As Integer ' Moves to a new cell
Declare Sub BlockWall(grid As GRID_TYPE,cell as Integer, wall as integer) ' Sets a bit on the cell border property preventing the ability to travel in a particular direction
Declare Function IsWallBlocked(grid As GRID_TYPE,cell as Integer, wall as integer) As Integer ' Checks if a wall is blocked
Declare Sub GenerateMaze(grid as GRID_TYPE) ' Carves out the maze
Declare Function Backtrack(grid As GRID_TYPE) As Integer ' Backtracks through the maze until a move can be made or until the maze is completed
Declare Sub SetHighestResolution() ' automatically adjusts to the highest resolution and goes full screen
Declare Function GetNextMove(grid as GRID_TYPE) As String ' finds a valid cell to move to if one is available
Declare Sub EraseCellWall(grid As GRID_TYPE, cellIndex as Integer, wall as Integer,c as Integer) ' draws a line to 'erase the wall'
Declare Sub ColorCell(grid As GRID_TYPE, cellIndex as Integer, c as Integer,bc as integer) ' draws a cell including border, c is fill color, bc is border color
Declare Sub DrawInitialGrid(grid as GRID_TYPE) 'Draws the grid and populates the grid.cells array


Dim grid AS GRID_TYPE
SetHighestResolution
SetMouse 0,0,0 'Hide Mouse Cursor

' populate grid structure
' Error checking should be performed here, but its not...
Input "Enter the x origin: ", grid.origin.x 
Input "Enter the y origin: ", grid.origin.y
Input "Enter the grid height: ", grid.height 
Input "Enter the grid width: ", grid.width 
Input "Enter the cell size: ", grid.cellSize
Print "Press any key to continue..."
Sleep
cls
grid.numberOfCells = grid.width * grid.height
grid.cellStack = CAllocate(grid.numberOfCells, sizeof(Integer) ) 'Allocate memory for stack
grid.cells = CAllocate(grid.numberOfCells, sizeof(CELL_TYPE) ) 'Allocate memory for cells
'--------------------------------------------------------------

DrawInitialGrid(grid) 
'sleep
GenerateMaze(grid) ' Carves the maze 
'sleep
paint(grid.cells[0].border.x1 + 1, grid.cells[0].border.y1 + 1),BLACK,WHITE
sleep
'Cleanup allocated memory
Deallocate grid.cells
Deallocate grid.cellStack 
end

Sub GenerateMaze(grid As GRID_TYPE)
    Dim validMoves as String
    Dim rndMove as Integer
    Dim currentCell as Integer

    'select first cell
    Randomize Timer, 5 ' 5 = Win32 Crypto API, Linux /dev/urandom)
    grid.currentCell = int(rnd * grid.numberOfCells)
    grid.cellStack[grid.cellStackIndex] = grid.currentCell
    ColorCell(grid, grid.currentCell,RED,WHITE)
    grid.numberOfCellsVisited += 1

Do
    Do
        validMoves = GetNextMove(grid)
        grid.cells[grid.currentCell].visited = true
        
        rndMove = Int(rnd *len(validMoves)) + 1
        grid.currentCell = MoveToNextCell(grid, Mid(validMoves,rndMove,1))
        ColorCell(grid, grid.currentCell,LIGHT_BLUE ,WHITE)
        'sleep 1
        grid.cellStack[grid.cellStackIndex] = grid.currentCell
        grid.cellStackIndex +=1
        
        if grid.cells[grid.currentCell].visited = 0 Then
            grid.numberOfCellsVisited += 1 ' Increase numberOfCellsVisited only if that cell has not been visited
        end if

Loop Until len(validMoves) = 0
        Backtrack(grid)
Loop until grid.numberOfCellsVisited = grid.numberOfCells
End Sub
'-------------------------------------------------------------------------------

'-------------------------------------------------------------------------------
Function GetNextMove(grid as GRID_TYPE) As String
    Dim validMoves As String = "" 'holds a list of moves that can be made
    ' if we can move in a direction
    ' and the cell has not been visited then we can
    ' append that move to validMoves
    ' U=UP D=DOWN L=LEFT R=RIGHT

    if IsWallBlocked(grid ,grid.currentCell,NORTH_WALL) = false then
        if grid.cells[grid.currentCell - grid.width].visited = false then
            validMoves += "U"
        end if
    end if
 
    if IsWallBlocked(grid, grid.currentCell,SOUTH_WALL) = false then
        if grid.cells[grid.currentCell + grid.width].visited = false then
            validMoves += "D"
        end if
    end if

    if IsWallBlocked(grid, grid.currentCell,WEST_WALL) = false then
        if grid.cells[grid.currentCell - 1].visited = false then
            validMoves += "L"
        end if
    end if

    if IsWallBlocked(grid, grid.currentCell,EAST_WALL) = false then
        if grid.cells[grid.currentCell + 1].visited = false then
            validMoves += "R"
        end if
    end if

    return validMoves
end Function
'-------------------------------------------------------------------------------

Function MoveToNextCell(grid As GRID_TYPE, move as String) As Integer
    Select Case move
    case "U"  ' move up
        grid.currentCell -= grid.width
        BlockWall(grid,grid.currentCell, SOUTH_WALL)
        eraseCellWall grid, grid.currentCell, SOUTH_WALL, 0
    case "D" ' move down
        grid.currentCell += grid.width
        BlockWall(grid, grid.currentCell, NORTH_WALL)
        eraseCellWall grid, grid.currentCell, NORTH_WALL, 0
    case "L" ' move left
        grid.currentCell -= 1
        BlockWall(grid, grid.currentCell, EAST_WALL)
        eraseCellWall grid, grid.currentCell, EAST_WALL, 0
    case "R" ' move right
        grid.currentCell += 1
        BlockWall(grid, grid.currentCell, WEST_WALL)
        eraseCellWall grid, grid.currentCell, WEST_WALL, 0
    end select
    
    return grid.currentCell
end function
'-------------------------------------------------------------------------------

'-------------------------------------------------------------------------------
Function Backtrack(grid As GRID_TYPE) As Integer
    Dim validMoves as String
    'backtrack loop
    for n as integer = grid.cellStackIndex -1 to 0 step - 1
        grid.currentCell = grid.cellStack[n]
        validMoves = GetNextMove(grid)
        ColorCell(grid, grid.currentCell,PURPLE,WHITE)
        'sleep 1
        ColorCell(grid, grid.currentCell,GREEN,WHITE)
        if len(validMoves) > 0 then
            grid.cellStackIndex = n
            Exit For
        end if
    next
    return grid.currentCell
End Function
'-------------------------------------------------------------------------------

Function IsWallBlocked(grid As GRID_TYPE, cell As Integer, wall as integer) as Integer
    return grid.cells[cell].borderProperties And wall
End Function
'-------------------------------------------------------------------------------

Sub BlockWall(grid As GRID_TYPE ,cell as Integer, wall as integer)
    grid.cells[cell].borderProperties or= wall
End Sub
'-------------------------------------------------------------------------------
Sub EraseCellWall(grid As GRID_TYPE, cellIndex as Integer, wall as Integer,c as Integer)
    Select Case wall
    case NORTH_WALL
        line(grid.cells[cellIndex].border.x1 + 1, _
        grid.cells[cellIndex].border.y1)-(_
        grid.cells[cellIndex].border.x2 - 1,_
        grid.cells[cellIndex].border.y1),c'erase top wall
    case SOUTH_WALL 
               line(grid.cells[cellIndex].border.x1 + 1, _
        grid.cells[cellIndex].border.y2)-(_
        grid.cells[cellIndex].border.x2 - 1,_
        grid.cells[cellIndex].border.y2),c'erase bottom wall
    case EAST_WALL 
               line(grid.cells[cellIndex].border.x2, _
        grid.cells[cellIndex].border.y1 + 1)-(_
        grid.cells[cellIndex].border.x2,_
        grid.cells[cellIndex].border.y2 - 1),c'erase right wall
    case WEST_WALL 
               line(grid.cells[cellIndex].border.x1, _
        grid.cells[cellIndex].border.y1 + 1)-(_
        grid.cells[cellIndex].border.x1,_
        grid.cells[cellIndex].border.y2 - 1),c'erase left wall
    End Select
End Sub
'-------------------------------------------------------------------------------

'-------------------------------------------------------------------------------
Sub ColorCell(grid As GRID_TYPE, cellIndex as Integer, c as Integer,bc as integer)
    Dim x as integer
    Dim y as integer
    
   if (cellIndex >= 0) And (cellIndex < grid.numberOfCells) then
       for x = grid.cells[cellIndex].border.x1 + 1 to grid.cells[cellIndex].border.x2 - 1
        for y = grid.cells[cellIndex].border.y1 + 1 to grid.cells[cellIndex].border.y2 - 1
            PSet(x,y),c
        next
       next
    End If
End Sub
'-------------------------------------------------------------------------------

'-------------------------------------------------------------------------------
Sub DrawInitialGrid(grid as GRID_TYPE)
    ' Draws the initial grid and assigns the appropriate wall blocks/boundaries
    Dim x as integer
    Dim y as integer
    Dim index as integer = 0

    for y = grid.origin.y to (((grid.height -1) * grid.cellSize) + grid.origin.y) step grid.cellSize
        for x = grid.origin.x to (((grid.width - 1) * grid.cellSize) + grid.origin.x) step grid.cellSize
        
            grid.cells[index].border.x1 = x
            grid.cells[index].border.y1 = y
            grid.cells[index].border.x2 = x + grid.cellsize
            grid.cells[index].border.y2 = y + grid.cellsize
            grid.cells[index].border.borderColor = WHITE
            
            if x = grid.origin.x then
                BlockWall(grid, index, WEST_WALL)
            end if
            
            if x = (((grid.width - 1) * grid.cellSize) + grid.origin.x) then
                BlockWall(grid, index, EAST_WALL)
            end if
            
            if y = grid.origin.y then
                BlockWall(grid, index, NORTH_WALL)
            end if
            
            if y = (((grid.height -1) * grid.cellSize) + grid.origin.y) then
                BlockWall(grid, index, SOUTH_WALL)
            end if
                
            line(grid.cells[index].border.x1, _
            grid.cells[index].border.y1)-(_
            grid.cells[index].border.x2,_
            grid.cells[index].border.y2),WHITE,b'draw the cell as an unfilled square
            index += 1
        next
    next
End Sub
'-------------------------------------------------------------------------------

Sub SetHighestResolution()
    Dim mode as integer
    Dim screen_height as integer
    Dim screen_width as integer
    
    mode = ScreenList(8)
    While (mode <> 0)
        screen_width = HiWord(mode)
        screen_height = LoWord(mode)
        mode = ScreenList()
    Wend

    Screenres screen_width, screen_height, 8, ,&h1
End Sub    
'-------------------------------------------------------------------------------
'EOF