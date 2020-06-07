{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server

import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

checkout :: MonadIO m => BookingId -> AppT m String
checkout bookingId = do
  bookings <- getBookings bookingId
  if (length bookings == 0) 
    then 
      return "Booking not found"
    else do
      let booking = bookings !! 0
      tryBook booking >>= \case
        Nothing -> return $ "Booking paid"
        Just errorr -> throwJSONError err400 $ errorr

refund :: MonadIO m => BookingId -> AppT m String
refund bookingId = do
  bookings <- getBookings bookingId
  case bookings of
    [] -> throwJSONError err404 $ JSONError $ "Booking not found"
    Booking newBookingId _ _ _ _ : _ -> do
      deleteBooking newBookingId
      return $ "Booking cancelled"