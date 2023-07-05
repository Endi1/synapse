-- migrate:up
ALTER TABLE synapses ADD COLUMN name TEXT NOT NULL;


-- migrate:down
ALTER TABLE synapses DROP COLUMN name;
