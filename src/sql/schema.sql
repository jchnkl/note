/* All tables are linked by id
 * CHAR(65): sha256 produces a 65 character long hex sum
 * CHAR(25): iso date (date -Iseconds)
 */

/* Table for note meta information */
CREATE TABLE IF NOT EXISTS info(
  id    CHAR(65) NOT NULL UNIQUE,
  date  CHAR(25) NOT NULL,
  title TEXT
);

);

/* Table for the actual note */
CREATE TABLE IF NOT EXISTS content(
  id   CHAR(65) NOT NULL UNIQUE,
  text BLOB,
  FOREIGN KEY(id) REFERENCES info(id)
);

/* Table for attachments */
CREATE TABLE IF NOT EXISTS attachments(
  id      CHAR(65) NOT NULL,
  content BLOB,
  FOREIGN KEY(id) REFERENCES info(id)
);
