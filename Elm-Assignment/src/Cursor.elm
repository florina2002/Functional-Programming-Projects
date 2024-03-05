module Cursor exposing (Cursor, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

type Cursor a
    = Cursor (List a) a (List a)


-- Create a cursor with a selected element from a list
withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right


-- Create a cursor from a non-empty list
nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs


-- Create a cursor from a list if it's not empty
fromList : List a -> Maybe (Cursor a)
fromList list =
    case list of
        [] ->
            -- The list is empty, so there's no cursor
            Nothing

        x :: xs ->
            -- Create a cursor with the first element selected
            Just (nonEmpty x xs)


-- Convert the cursor to a list
toList : Cursor a -> List a
toList (Cursor left mid right) =
    -- Concatenate the reversed left, the selected element, and the right
    List.reverse left ++ mid :: right


-- Get the currently selected element from the cursor
current : Cursor a -> a
current (Cursor _ a _) =
    a


-- Move the cursor forward
forward : Cursor a -> Maybe (Cursor a)
forward (Cursor left mid right) =
    case right of
        [] ->
            -- Cannot move forward if at the last element
            Nothing

        x :: xs ->
            -- Move the selected element to the right and select the next element
            Just (Cursor (mid :: left) x xs)





-- Move the cursor backward
back : Cursor a -> Maybe (Cursor a)
back (Cursor left mid right) =
    case left of
        [] ->
            -- Cannot move backward if at the first element
            Nothing

        x :: xs ->
            -- Move the selected element to the right and select the previous element
            Just (withSelectedElement xs x (mid :: right))


-- Get the number of elements in the cursor
length : Cursor a -> Int
length (Cursor left _ right) =
    -- Calculate the total length by summing the lengths of left and right, plus 1 for the selected element
    List.length left + 1 + List.length right




