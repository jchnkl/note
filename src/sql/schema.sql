/* All tables are linked by note_meta_id
 * CHAR(65): sha256 produces a 65 character long hex sum
 * CHAR(25): iso date (date -Iseconds)
 */

/* Table for note meta information */
CREATE TABLE IF NOT EXISTS notes_meta(
  note_meta_id   CHAR(65) NOT NULL UNIQUE,
  note_meta_date CHAR(25),
  note_meta_name TEXT
);

/* Table for the actual note */
CREATE TABLE IF NOT EXISTS notes_note(
  note_id      CHAR(65) NOT NULL UNIQUE,
  note_content BLOB,
  FOREIGN KEY(note_id) REFERENCES notes_meta(note_meta_id)
);

/* Table for attachments */
CREATE TABLE IF NOT EXISTS notes_attachments(
  attachment_id      CHAR(65) NOT NULL,
  attachment_content BLOB,
  FOREIGN KEY(attachment_id) REFERENCES notes_meta(note_meta_id)
);
