module Common where

data MyWorkspace
    = Cmd
    | Web
    | Code
    | Doc
    | Note
    | Com
    | Media
    | Remote
    | Float
    deriving (Show, Eq, Enum, Bounded)

wsName :: MyWorkspace -> String
wsName Cmd    = "\61728"
wsName Web    = "\62845"
wsName Code   = "\61729"
wsName Doc    = "\62744"
wsName Note   = "\61614"
wsName Com    = "\61664"
wsName Media  = "\63612"
wsName Remote = "\61705"
wsName Float  = "\62029"

myWorkspaces = [minBound::MyWorkspace .. maxBound]

myWorkspaceNames = wsName <$> myWorkspaces
