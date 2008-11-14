CREATE TABLE score(
  score VARCHAR(4) PRIMARY KEY);

INSERT INTO score VALUES ('+');
INSERT INTO score VALUES ('O');
INSERT INTO score VALUES ('O+');
INSERT INTO score VALUES ('OO');
INSERT INTO score VALUES ('OO+');
INSERT INTO score VALUES ('OOO');
INSERT INTO score VALUES ('OOO+');
INSERT INTO score VALUES ('OOOO');

CREATE TABLE book(
 title VARCHAR(64),
 author VARCHAR(64),
 score VARCHAR(4) REFERENCES score (score),
 PRIMARY KEY (author, title)
);

INSERT INTO book VALUES ('Expert Python Programming', 'Ziade', 'OOO+');
