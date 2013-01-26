PRAGMA foreign_keys = ON;

CREATE TABLE player (
  playerId INTEGER PRIMARY KEY AUTOINCREMENT,
  playerName TEXT UNIQUE
  );

CREATE TABLE event (
  eventId         INTEGER PRIMARY KEY AUTOINCREMENT,
  previousEventId INTEGER,
  eventName       TEXT    NOT NULL,
  eventDay        TEXT    NOT NULL,
  eventActive     INTEGER NOT NULL,
  FOREIGN KEY (previousEventId) REFERENCES event
  );

CREATE TABLE law (
  playerId INTEGER NOT NULL REFERENCES player,
  eventId  INTEGER NOT NULL REFERENCES event,
  mean     REAL NOT NULL,
  stddev   REAL NOT NULL,
  lawData  TEXT NOT NULL,
  PRIMARY KEY (playerId, eventId)
  );

CREATE TABLE match (
  matchId  INTEGER PRIMARY KEY AUTOINCREMENT,
  eventId  INTEGER NOT NULL REFERENCES event,
  winnerId INTEGER NOT NULL REFERENCES player,
  loserId INTEGER NOT NULL REFERENCES player,
  matchTime TEXT
  );
