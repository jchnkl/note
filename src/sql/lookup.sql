-- id = sha256 (unique)
-- return: (id, date, content)
-- id = sha256 (unique)
-- date = date -I seconds
-- content = freeform text
SELECT * FROM info where id = ?;
