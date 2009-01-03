CREATE TABLE bookid (
 id INTEGER PRIMARY KEY,
 title VARCHAR(128),
 author VARCHAR(64),
 rdate DATETIME,
 UNIQUE (title, author, rdate)
);

CREATE TABLE score (
 score VARCHAR(4) PRIMARY KEY,
 value INTEGER);

INSERT INTO score VALUES ('+', 1);
INSERT INTO score VALUES ('O', 2);
INSERT INTO score VALUES ('O+', 3);
INSERT INTO score VALUES ('OO', 4);
INSERT INTO score VALUES ('OO+', 5);
INSERT INTO score VALUES ('OOO', 6);
INSERT INTO score VALUES ('OOO+', 7);
INSERT INTO score VALUES ('OOOO', 8);

CREATE TABLE genre (
 id CHAR(2) PRIMARY KEY,
 descr VARCHAR(32))

CREATE TABLE nation (
 id CHAR(2) PRIMARY KEY,
 descr VARCHAR(32));

CREATE TABLE book (
 id INTEGER PRIMARY KEY REFERENCES bookid (id),
 genre CHAR(2) REFERENCES genre (id),
 nation CHAR(2) REFERENCES nation (id),
 score INTEGER REFERENCES score (value));

CREATE VIEW book_view AS
 SELECT a.id, a.title, a.author, a.rdate, b.genre, b.nation, c.score 
 FROM bookid AS a
 INNER JOIN book AS b
 ON a.id = b.id
 INNER JOIN score AS c
 ON c.value=b.score;
