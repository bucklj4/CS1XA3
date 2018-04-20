module UnsafeToPure where
import qualified Control.Exception as Exc
import System.IO.Unsafe

-- Code from https://stackoverflow.com/questions/4243117/how-to-catch-and-ignore-a-call-to-the-error-function
unsafeCleanup :: a -> Maybe a
unsafeCleanup x = unsafePerformIO $ Exc.catch (x `seq` return (Just x)) handler
    where
    handler exc = return Nothing  `const`  (exc :: Exc.ErrorCall)