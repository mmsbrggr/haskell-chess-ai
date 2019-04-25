module Evaluation.Tables
    ( pawn
    , knight
    , bishop
    , rook
    , queen
    , kingMiddle
    , kingEnd
    )
where

import           Board
import qualified Data.Vector.Unboxed           as V

pawn :: Color -> V.Vector Int
pawn White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , 10
    , 10
    , -20
    , -20
    , 10
    , 10
    , 5
    , 0
    , 0
    , 5
    , -5
    , -10
    , 0
    , 0
    , -10
    , -5
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , 5
    , 10
    , 25
    , 25
    , 10
    , 5
    , 5
    , 0
    , 0
    , 10
    , 10
    , 20
    , 30
    , 30
    , 20
    , 10
    , 10
    , 0
    , 0
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
pawn Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 50
    , 0
    , 0
    , 10
    , 10
    , 20
    , 30
    , 30
    , 20
    , 10
    , 10
    , 0
    , 0
    , 5
    , 5
    , 10
    , 25
    , 25
    , 10
    , 5
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , -5
    , -10
    , 0
    , 0
    , -10
    , -5
    , 5
    , 0
    , 0
    , 5
    , 10
    , 10
    , -20
    , -20
    , 10
    , 10
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

knight :: Color -> V.Vector Int
knight White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -50
    , -40
    , -30
    , -30
    , -30
    , -30
    , -40
    , -50
    , 0
    , 0
    , -40
    , -20
    , 0
    , 5
    , 5
    , 0
    , -20
    , -40
    , 0
    , 0
    , -30
    , 5
    , 10
    , 15
    , 15
    , 10
    , 5
    , -30
    , 0
    , 0
    , -30
    , 0
    , 15
    , 20
    , 20
    , 15
    , 0
    , -30
    , 0
    , 0
    , -30
    , 5
    , 15
    , 20
    , 20
    , 15
    , 5
    , -30
    , 0
    , 0
    , -30
    , 0
    , 10
    , 15
    , 15
    , 10
    , 0
    , -30
    , 0
    , 0
    , -40
    , -20
    , 0
    , 0
    , 0
    , 0
    , -20
    , -40
    , 0
    , 0
    , -50
    , -40
    , -30
    , -30
    , -30
    , -30
    , -40
    , -50
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
knight Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -50
    , -40
    , -30
    , -30
    , -30
    , -30
    , -40
    , -50
    , 0
    , 0
    , -40
    , -20
    , 0
    , 0
    , 0
    , 0
    , -20
    , -40
    , 0
    , 0
    , -30
    , 0
    , 10
    , 15
    , 15
    , 10
    , 0
    , -30
    , 0
    , 0
    , -30
    , 5
    , 15
    , 20
    , 20
    , 15
    , 5
    , -30
    , 0
    , 0
    , -30
    , 0
    , 15
    , 20
    , 20
    , 15
    , 0
    , -30
    , 0
    , 0
    , -30
    , 5
    , 10
    , 15
    , 15
    , 10
    , 5
    , -30
    , 0
    , 0
    , -40
    , -20
    , 0
    , 5
    , 5
    , 0
    , -20
    , -40
    , 0
    , 0
    , -50
    , -40
    , -30
    , -30
    , -30
    , -30
    , -40
    , -50
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

bishop :: Color -> V.Vector Int
bishop White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -20
    , -10
    , -10
    , -10
    , -10
    , -10
    , -10
    , -20
    , 0
    , 0
    , -10
    , 5
    , 0
    , 0
    , 0
    , 0
    , 5
    , -10
    , 0
    , 0
    , -10
    , 10
    , 10
    , 10
    , 10
    , 10
    , 10
    , -10
    , 0
    , 0
    , -10
    , 0
    , 10
    , 10
    , 10
    , 10
    , 0
    , -10
    , 0
    , 0
    , -10
    , 5
    , 5
    , 10
    , 10
    , 5
    , 5
    , -10
    , 0
    , 0
    , -10
    , 0
    , 5
    , 10
    , 10
    , 5
    , 0
    , -10
    , 0
    , 0
    , -10
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -20
    , -10
    , -10
    , -10
    , -10
    , -10
    , -10
    , -20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
bishop Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -20
    , -10
    , -10
    , -10
    , -10
    , -10
    , -10
    , -20
    , 0
    , 0
    , -10
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -10
    , 0
    , 5
    , 10
    , 10
    , 5
    , 0
    , -10
    , 0
    , 0
    , -10
    , 5
    , 5
    , 10
    , 10
    , 5
    , 5
    , -10
    , 0
    , 0
    , -10
    , 0
    , 10
    , 10
    , 10
    , 10
    , 0
    , -10
    , 0
    , 0
    , -10
    , 10
    , 10
    , 10
    , 10
    , 10
    , 10
    , -10
    , 0
    , 0
    , -10
    , 5
    , 0
    , 0
    , 0
    , 0
    , 5
    , -10
    , 0
    , 0
    , -20
    , -10
    , -10
    , -10
    , -10
    , -10
    , -10
    , -20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

rook :: Color -> V.Vector Int
rook White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , 5
    , 10
    , 10
    , 10
    , 10
    , 10
    , 10
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
rook Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , 10
    , 10
    , 10
    , 10
    , 10
    , 10
    , 5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , 5
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

queen :: Color -> V.Vector Int
queen White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -20
    , -10
    , -10
    , -5
    , -5
    , -10
    , -10
    , -20
    , 0
    , 0
    , -10
    , 0
    , 5
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -10
    , 5
    , 5
    , 5
    , 5
    , 5
    , 0
    , -10
    , 0
    , 0
    , 0
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -5
    , 0
    , 0
    , -5
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -5
    , 0
    , 0
    , -10
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -10
    , 0
    , 0
    , -10
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -20
    , -10
    , -10
    , -5
    , -5
    , -10
    , -10
    , -20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
queen Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -20
    , -10
    , -10
    , -5
    , -5
    , -10
    , -10
    , -20
    , 0
    , 0
    , -10
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -10
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -10
    , 0
    , 0
    , -5
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -5
    , 0
    , 0
    , 0
    , 0
    , 5
    , 5
    , 5
    , 5
    , 0
    , -5
    , 0
    , 0
    , -10
    , 5
    , 5
    , 5
    , 5
    , 5
    , 0
    , -10
    , 0
    , 0
    , -10
    , 0
    , 5
    , 0
    , 0
    , 0
    , 0
    , -10
    , 0
    , 0
    , -20
    , -10
    , -10
    , -5
    , -5
    , -10
    , -10
    , -20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

kingMiddle :: Color -> V.Vector Int
kingMiddle White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 20
    , 30
    , 10
    , 0
    , 0
    , 10
    , 30
    , 20
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , -10
    , -20
    , -20
    , -20
    , -20
    , -20
    , -20
    , -10
    , 0
    , 0
    , -20
    , -30
    , -30
    , -40
    , -40
    , -30
    , -30
    , -20
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
kingMiddle Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -30
    , -40
    , -40
    , -50
    , -50
    , -40
    , -40
    , -30
    , 0
    , 0
    , -20
    , -30
    , -30
    , -40
    , -40
    , -30
    , -30
    , -20
    , 0
    , 0
    , -10
    , -20
    , -20
    , -20
    , -20
    , -20
    , -20
    , -10
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , 0
    , 0
    , 20
    , 20
    , 0
    , 0
    , 20
    , 30
    , 10
    , 0
    , 0
    , 10
    , 30
    , 20
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

kingEnd :: Color -> V.Vector Int
kingEnd White = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -50
    , -30
    , -30
    , -30
    , -30
    , -30
    , -30
    , -50
    , 0
    , 0
    , -30
    , -30
    , 0
    , 0
    , 0
    , 0
    , -30
    , -30
    , 0
    , 0
    , -30
    , -10
    , 20
    , 30
    , 30
    , 20
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 30
    , 40
    , 40
    , 30
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 30
    , 40
    , 40
    , 30
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 20
    , 30
    , 30
    , 20
    , -10
    , -30
    , 0
    , 0
    , -30
    , -20
    , -10
    , 0
    , 0
    , -10
    , -20
    , -30
    , 0
    , 0
    , -50
    , -40
    , -30
    , -20
    , -20
    , -30
    , -40
    , -50
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]
kingEnd Black = V.fromList
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , -50
    , -40
    , -30
    , -20
    , -20
    , -30
    , -40
    , -50
    , 0
    , 0
    , -30
    , -20
    , -10
    , 0
    , 0
    , -10
    , -20
    , -30
    , 0
    , 0
    , -30
    , -10
    , 20
    , 30
    , 30
    , 20
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 30
    , 40
    , 40
    , 30
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 30
    , 40
    , 40
    , 30
    , -10
    , -30
    , 0
    , 0
    , -30
    , -10
    , 20
    , 30
    , 30
    , 20
    , -10
    , -30
    , 0
    , 0
    , -30
    , -30
    , 0
    , 0
    , 0
    , 0
    , -30
    , -30
    , 0
    , 0
    , -50
    , -30
    , -30
    , -30
    , -30
    , -30
    , -30
    , -50
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]