module Queue ( Queue, emptyQueue, isEmptyQueue, enQueue, deQueue, front ) where

-------------------------------------------------------------------------------
-- I N T E R F A C E  :  P U B L I C
-------------------------------------------------------------------------------

-- Queue a : a first-in first-out collection of items of type 'a'

-------------------------------------------------------------------------------

---- emptyQueue : the empty queue

emptyQueue :: Queue a

------------------------------------------------------------------------------

---- isEmptyQueue q : is queue 'q' empty ?

isEmptyQueue :: Queue a -> Bool

------------------------------------------------------------------------------

---- enQueue x q : the queue formed by placing item 'x' at the back of queue 'q'

enQueue :: a -> Queue a -> Queue a

------------------------------------------------------------------------------

---- dequeue q : the queue formed by removing its front item
----             from the non-empty queue 'q'

deQueue :: Queue a -> Queue a

------------------------------------------------------------------------------

---- front q : the front item of the non-empty queue 'q'

front :: Queue a -> a

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- I M P L E M E N T A T I O N  :  P R I V A T E
------------------------------------------------------------------------------

data Queue a = EmptyQueue | Single a | Q a ( Queue a ) a deriving (Show)

-------------------------------------------------------------------------------
-- q is a Queue
-- qf is the front item of a Queue
-- qb is the back item of a Queue
-----------------------------------------------------------------------------

emptyQueue = EmptyQueue 

-------------------------------------------------------------------------

isEmptyQueue EmptyQueue = True
isEmptyQueue _ = False

-------------------------------------------------------------------------

enQueue x EmptyQueue = Single x
enQueue x (Single q) = Q q (EmptyQueue) x
enQueue x (Q qf q qb) =  Q qf (enQueue qb q) x

-------------------------------------------------------------------------

deQueue (Single _) = EmptyQueue
deQueue (Q _ q qb) = enQueue qb q

-------------------------------------------------------------------------

front (Single q) = q
front (Q qf _ _) = qf

---------------------------------------------------
