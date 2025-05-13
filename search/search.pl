%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Full Prolog Code - Updated Version
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Entry point for searching the sequence of actions
search(Actions) :-
    initial(Start),
    % Collect any keys in the starting room
    findall(Color, key(Start, Color), KeyList0),
    sort(KeyList0, Keys0),
    % Begin BFS with initial node: Room, KeysHeld, ActionPath
    bfs([node(Start, Keys0, [])], [], Actions).

% Acquire all keys in a room (if any) and merge with existing keys
acquire_key(Room, Keys, KeysNew) :-
    findall(Color, key(Room, Color), NewKeys),
    append(Keys, NewKeys, KeysAgg),
    sort(KeysAgg, KeysNew).

% Define an unlocked move: pass through an unguarded door
unlocked(Room, Keys, Next, KeysNew, [move(Room, Next)]) :-
    ( door(Room, Next)
    ; door(Next, Room)
    ),
    acquire_key(Next, Keys, KeysNew).

% Define a locked move: unlock then move if key is held
locked(Room, Keys, Next, KeysNew, [unlock(Color), move(Room, Next)]) :-
    ( locked_door(Room, Next, Color)
    ; locked_door(Next, Room, Color)
    ),
    memberchk(Color, Keys),  % Ensure the key is available
    acquire_key(Next, Keys, KeysNew).

% Generate successor nodes: node(Room, Keys, Path) -> node(Next, KeysNew, PathNew)
successor(node(Room, Keys, Path), node(Next, KeysNew, PathNew)) :-
    (   unlocked(Room, Keys, Next, KeysNew, Acts)
    ;   locked(Room, Keys, Next, KeysNew, Acts)
    ),
    append(Path, Acts, PathNew).

% BFS terminating condition: found the treasure
bfs([node(Room, _, Path)|_], _, Actions) :-
    treasure(Room),  % If current room is the treasure room
    Path = Actions.  % Keep the path as is

% BFS expansion: dequeue head, enqueue successors
bfs([Node|Rest], Visited, Actions) :-
    findall(NextNode,
        ( successor(Node, NextNode),
          NextNode = node(R2, K2, _),
          \+ memberchk(state(R2, K2), Visited)  % Avoid revisiting the same state
        ),
        NextNodes),
    % Extract new state signatures to avoid revisiting
    findall(state(R2, K2), member(node(R2, K2, _), NextNodes), NewStates),
    append(Visited, NewStates, Visited2),  % Add newly visited states to Visited list
    append(Rest, NextNodes, Queue2),
    bfs(Queue2, Visited2, Actions).
