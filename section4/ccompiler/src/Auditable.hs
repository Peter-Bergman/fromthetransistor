module Auditable where

class Auditable astNode where
    -- All of the methods below return True if pass, False if fail

    -- audit AST node and its descendents
    checkConstraintsOfNodeAndDescendents :: astNode -> Bool
    checkConstraintsOfNodeAndDescendents node = nodeFitsConstraints && descendentNodesFitConstraints
        where
            nodeFitsConstraints = checkConstraintsOfNode node
            descendentNodesFitConstraints = checkConstraintsOfDescendentNodes node

    -- audit AST node
    checkConstraintsOfNode :: astNode -> Bool

    -- audit descendent nodes of AST node recursively
    checkConstraintsOfDescendentNodes :: astNode -> Bool

