CREATE TABLE score(
  score VARCHAR(4) PRIMARY KEY);

CREATE TABLE book(
 title VARCHAR(64),
 author VARCHAR(64),
 score VARCHAR(4) REFERENCES score (score),
 PRIMARY KEY (author, title)
);
