module TournamentSummaryHtml where

import Law
import Data.List (sortBy)
import Data.Foldable (foldMap)
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Tournament
import Text.XHtml
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

tournamentColumns = 4

ratingsHtml :: Map String (Day, Law) -> String
ratingsHtml laws = renderHtml
  [ header <<
    [ thetitle << "Ratings"
    , thelink ! [rel "stylesheet", thetype "text/css", href "ratings.css" ] << noHtml
    ]
  , body <<
    [ h1 << "Rated Players"
    , ratingsTable byScore rated
    , h1 << "Unrated Players"
    , ratingsTable byName unrated
    ]
  ]
  where
  (rated, unrated) = Map.partition (lawIsRated . snd) laws
  byScore = flip (comparing (lawScore . snd . snd))
  byName  = comparing fst

ratingsTable :: ((String, (Day, Law)) -> (String, (Day, Law)) -> Ordering) -> Map String (Day, Law) -> Html
ratingsTable metric laws = table <<
  [ tr << [ th << "Player", th << "Rating", th << "Metric", th << "Last played" ]
  , ifoldMap ratingRow ordered
  ]
  where
  ratingRow i (name,(day,law)) =
    let classes
          | even i = []
          | otherwise = [theclass "alt"]
    in tr ! classes <<
    [ td << name
    , td << formatLaw law
    , td << showRound (lawScore law)
    , td << show day
    ]
  ordered = sortBy metric $ Map.toList laws


tournamentHtml :: Map String Law -> Day -> TournamentSummary String -> String
tournamentHtml laws day results = renderHtml
  [ header <<
    [ thetitle << title
    , thelink ! [rel "stylesheet", thetype "text/css", href "results.css" ] << noHtml
    ]
  , body <<
    [ h1 << title
    , tournamentTable laws results
    ]
  ]
  where
  title = formatTournamentTitle day

formatTournamentTitle :: Day -> String
formatTournamentTitle day
  = "Tournament Results - "
  ++ weekday ++ ", "
  ++ month ++ " " ++ show dayNum ++ ", "
  ++ show yearNum
  where
  (yearNum,monthNum,dayNum) = toGregorian day
  (_,_,weekdayNum) = toWeekDate day

  month = case monthNum of
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"

  weekday = case weekdayNum of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"

tableGroup _ [] = noHtml
tableGroup n xs =
  tr << map (td <<) a
  +++
  tableGroup n b
  where
  (a,b) = splitAt n xs

tournamentTable :: Map String Law -> TournamentSummary String -> Html
tournamentTable laws results =
  table ! [theclass "results"]
  << tableGroup tournamentColumns (map (uncurry formatPlayerResult) (Map.toList results))
  where
  formatPlayerResult playerName (finalLaw, matches) =
    thediv ! [theclass "resultbox"] <<
    [ thespan ! [theclass "playername"] << playerName
    , thespan ! [theclass "lawchange" ] << formatLawChange initialLaw finalLaw
    , summaryTable matches
    ]
    where
    initialLaw = getLaw playerName laws

summaryTable :: Map String MatchSummary -> Html
summaryTable summaries = table ! [theclass "matchbox"]
  <<
  [ tr << [ th << "Δ"
          , th << "Rating"
          , th << "Opponent"
          , th << "W-L"
          ]
  , ifoldMap mkRow $ Map.toList summaries
  ]
  where
  mkRow i (opponentName,summary) =
    tr ! classes <<
    [ td ! [theclass "delta"]
         << formatDelta (summaryPointChange summary)
    , td ! [theclass "rating"]
         << formatLaw   (summaryAdjustedLaw summary)
    , td ! [theclass "opponent"]
         << opponentName
    , td ! [theclass "outcome"]
         << formatOutcome (summaryOutcome summary)
    ]
    where
    classes
      | even i = []
      | otherwise = [theclass "alt"]

showRound :: Double -> String
showRound x = show (round x :: Integer)

formatLaw :: Law -> String
formatLaw law = showRound (lawMean law) ++ "±" ++ showRound (lawStddev law)

formatDelta :: Double -> String
formatDelta d
  | d > 0 = '+':showRound d
  | otherwise = showRound d

formatDeltaOp :: Double -> String
formatDeltaOp d
  | d >= 0 = " + " ++ showRound d
  | otherwise = " - " ++ showRound (- d)

-- | Render the change between an old law and a new law.
formatLawChange ::
  Law {- ^ Old law -} ->
  Law {- ^ New law -} ->
  String
formatLawChange old new =
  formatLaw old
  ++ formatDeltaOp (lawMean new - lawMean old) ++ " = "
  ++ formatLaw new

formatOutcome :: Outcome -> String
formatOutcome outcome = show (view outcomeWins outcome)
                     ++ "-"
                     ++ show (view outcomeLoses outcome)
