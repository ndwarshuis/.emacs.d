create or replace function to_localtime(ts timestamp with time zone)
returns time with time zone
as $$
  select ts::time at time zone 'US/Eastern';
$$ language SQL;

create or replace procedure make_vis_tables()
language plpgsql
as $$

declare
begin

create schema if not exists viz;

create temporary table _scheduled_timestamps as
select ts.* from planning_entries pe
  join timestamps ts using (timestamp_id)
  where
    pe.planning_type = 'scheduled';

create temporary table _deadline_timestamps as
select ts.* from planning_entries pe
  join timestamps ts using (timestamp_id)
  where
    pe.planning_type = 'deadline';

create temporary table _closed_timestamps as
select ts.* from planning_entries pe
  join timestamps ts using (timestamp_id)
  where
    pe.planning_type = 'closed';

create temporary table _tags as
select hc.headline_id, t.tag from headline_tags t
  join headline_closures hc on hc.parent_id = t.headline_id
  union
  select h.headline_id, t.tag from file_tags t
  join headlines h on h.outline_hash = t.outline_hash;

create temporary table _category_tags as
select distinct * from _tags t where t.tag like '\__%';

-- TODO there is a small chance that headlines might have two context tags
-- if different contexts are explicitly specified on two headlines at different
-- levels
create temporary table _context_tags as
select distinct * from _tags t where t.tag like '@_%';

create temporary table _resource_tags as
select distinct t.headline_id, array_agg(t.tag) as tags
  from _tags t
  where
    t.tag like '#_%'
  group by t.headline_id;

create temporary table _incubated_headlines as
select distinct * from _tags t
  where
    t.tag = '%inc';

create temporary table _other_tags as
select distinct t.headline_id, array_agg(t.tag) as tags
  from _tags t
  -- TODO this is not robust code; change will require edits in two places :(
  where
    not t.tag like '#_%'
    and not t.tag like '@_%'
    and not t.tag like '\__%'
    and t.tag <> '%inc'
  group by t.headline_id;

create temporary table _created_timestamps as
select
  hp.headline_id,
  to_timestamp(p.val_text, '[YYYY-MM-DD Dy HH24:MI]') as created_timestamp
  from headline_properties hp
  join properties p using (property_id)
  where
    p.key_text = 'CREATED'
    and not p.val_text is NULL;

-- temp table to hold all headlines with scheduled repeaters
create temporary table _repeaters as
select * from headlines h
  join _scheduled_timestamps ts using (headline_id)
  join timestamp_repeaters tr using (timestamp_id);

create temporary table _todo_closures as
select
  parents.headline_id as parent_id,
  children.headline_id as child_id,
  hc.depth as depth
  from headline_closures hc
  join headlines parents on parents.headline_id = hc.parent_id 
  join headlines children on children.headline_id = hc.headline_id 
  where
    -- this ensures we only get headlines that are children of other headlines
    hc.depth > 0
    -- the parents must have a keyword
    and not parents.keyword is NULL
    -- the children must have a keyword
    and not children.keyword is NULL;

create temporary table _iterator_headlines as
select distinct
  children.headline_id,
  children.headline_text,
  children.keyword
  from headline_closures hc
  join headlines parents on parents.headline_id = hc.parent_id
  join headlines children on children.headline_id = hc.headline_id
  join headline_properties parent_hp on parents.headline_id = parent_hp.headline_id
  join properties parent_props on parent_hp.property_id = parent_props.property_id
  where
    hc.depth > 0
    and parent_props.key_text = 'PARENT_TYPE'
    and parent_props.val_text = 'iterator';

create temporary table _clock_sums as
select
  c.headline_id,
  to_timestamp(c.time_start) as clock_start,
  to_timestamp(c.time_end) as clock_end,
  sum(c.time_end - c.time_start) / 60.0 as clock_sum
  from clocks c
  group by c.headline_id, c.time_start, c.time_end;

-- clock sums partitioned by DONE -> TODO state changes (this mostly useful for
-- repeaters although all headlines are included in this calculation)
create temporary table _partitioned_clock_sums as
-- with
--   -- this table will have all the clock times with the done->todo state changes
--   -- inserted in between (sorted by clock start time/state change time)
--   tmp as (
-- 	select
--       c.file_path,
--       c.headline_offset,
--       c.time_start,
--       c.time_end,
--       NULL as state_change_offset
--       from clocks c
--     union
--     select
--       le.file_path,
--       le.headline_offset,
--       le.time_logged as time_start,
--       NULL as time_end,
--       le.entry_offset as state_change_offset
--       from logbook_entries le
--       join state_changes sc using (file_path, entry_offset)
--       where
--         sc.state_old = 'TODO'
--         and (sc.state_new = 'DONE' or sc.state_new = 'CANC')
--   ),
--   -- this table will number each "group" of timestamps, where a "group" is
--   -- defined by timestamps under the same headline (eg matching file_path and
--   -- headline_offset) that are partitioned by the todo->done state change
--   -- entries (if any, if only one or none, there will only be one group under
--   -- one headline)
--   --
--   -- NOTE 1: the WHERE clause is in the next outer query since state-change rows
--   -- themselves (which are removed by the WHERE) are necessary to define the
--   -- groups)
--   -- 
--   -- NOTE 2: if a headline does not have any state changes, it may get the same
--   -- group index as the last group of the previous headline. This shouldn't
--   -- matter, since the GROUP BY in the outer query also takes the file_path and
--   -- headline_offset into account
--   grouped as (
-- 	select
--       t.file_path, 
-- 	  t.headline_offset,
-- 	  t.time_start,
-- 	  t.time_end,
-- 	  t.state_change_offset,
-- 	  sum(case when t.state_change_offset is not null then 1 end)
-- 	  over (order by t.file_path,
--               t.headline_offset,
--               t.time_start desc,
--               t.state_change_offset desc)
-- 	  as grp
--       from tmp t
--   ),
--   offsets as (
--     select
--       g.file_path,
--       g.headline_offset,
--       g.grp,
--       g.state_change_offset
--       from grouped g
--     where
--       not g.state_change_offset is NULL
--   )
--   select
--     g.file_path,
--     g.headline_offset,
--     min(g.time_start) as partitioned_time_start,
--     max(g.time_end) as partitioned_time_end,
--     sum(g.time_end - g.time_start) / 60.0 as partitioned_clock_sum,
--     o.state_change_offset
--     from grouped g
--   right join offsets o using (file_path, headline_offset, grp)
--   where
--     g.state_change_offset is NULL
--   group by g.file_path, g.headline_offset, g.grp, o.state_change_offset;

with
  -- this table will have all the clock times with the done->todo state changes
  -- inserted in between (sorted by clock start time/state change time)
  tmp as (
	select
      c.headline_id,
      c.time_start,
      c.time_end,
      NULL as state_change_id
      from clocks c
    union
    select
      le.headline_id,
      le.time_logged as time_start,
      NULL as time_end,
      le.entry_id as state_change_id
      from logbook_entries le
      join state_changes sc using (entry_id)
      where
        sc.state_old = 'TODO'
        and (sc.state_new = 'DONE' or sc.state_new = 'CANC')
  ),
  -- this table will number each "group" of timestamps, where a "group" is
  -- defined by timestamps under the same headline (eg matching file_path and
  -- headline_offset) that are partitioned by the todo->done state change
  -- entries (if any, if only one or none, there will only be one group under
  -- one headline)
  --
  -- NOTE 1: the WHERE clause is in the next outer query since state-change rows
  -- themselves (which are removed by the WHERE) are necessary to define the
  -- groups)
  -- 
  -- NOTE 2: if a headline does not have any state changes, it may get the same
  -- group index as the last group of the previous headline. This shouldn't
  -- matter, since the GROUP BY in the outer query also takes the file_path and
  -- headline_offset into account
  grouped as (
	select
	  t.headline_id,
	  t.time_start,
	  t.time_end,
	  t.state_change_id,
	  sum(case when t.state_change_id is not null then 1 end)
	  over (order by t.headline_id, t.time_start desc, t.state_change_id desc)
	  as grp
      from tmp t
  ),
  ids as (
    select g.headline_id, g.grp, g.state_change_id from grouped g
    where
      not g.state_change_id is NULL
  ),
  sums as (
	select
      g.headline_id,
	  g.grp,
      to_timestamp(min(g.time_start)) as partitioned_clock_start,
      to_timestamp(max(g.time_end)) as partitioned_clock_end,
      sum(g.time_end - g.time_start) / 60.0 as partitioned_clock_sum
      from grouped g
    where
      g.state_change_id is NULL
    group by g.headline_id, g.grp
  )
  select
    o.headline_id,
    o.state_change_id,
    s.partitioned_clock_start,
    s.partitioned_clock_end,
    s.partitioned_clock_sum
    from ids o
  left join sums s using (headline_id, grp);

create temporary table _habit_headlines as
select
  r.*,
  cs.partitioned_clock_start,
  cs.partitioned_clock_end,
  cs.partitioned_clock_sum,
  s.state_old,
  s.state_new,
  to_timestamp(le.time_logged) as closed_timestamp
  from _repeaters r
  join headline_properties hp using (headline_id)
  join properties p using (property_id)
  join _partitioned_clock_sums cs using (headline_id)
  left join state_changes s
  on s.entry_id = cs.state_change_id
  left join logbook_entries le
  on le.headline_id = r.headline_id and le.entry_id = s.entry_id
  where
    p.key_text = 'STYLE'
    and p.val_text = 'habit';

create temporary table _repeater_headlines as
select
  r.*,
  cs.partitioned_clock_start,
  cs.partitioned_clock_end,
  cs.partitioned_clock_sum,
  s.state_old,
  s.state_new,
  to_timestamp(le.time_logged) as closed_timestamp
  from _repeaters r
  join _partitioned_clock_sums cs using (headline_id)
  left join state_changes s
  on s.entry_id = cs.state_change_id
  left join logbook_entries le
  on le.headline_id = r.headline_id and le.entry_id = s.entry_id
  where
    not exists (select * from _habit_headlines habits
      where r.headline_id = habits.headline_id);

create temporary table _project_task_headlines as
select distinct h.* from _todo_closures tc
  join headlines h on tc.child_id = h.headline_id
  where
    not exists (select * from _iterator_headlines i
                 where i.headline_id = h.headline_id);

-- drop table if exists viz.project_toplevel_headlines;
-- create table viz.project_toplevel_headlines as
-- select distinct
--   h.file_path,
--   h.headline_offset,
--   h.keyword,
--   h.headline_text
--   from _todo_closures tc0
--   join headlines h
--   on tc0.file_path = h.file_path and tc0.parent_offset = h.headline_offset
--   where
--     tc0.depth = 1
--     and not exists (select * from _todo_closures tc1
--                    where tc1.child_offset = tc0.parent_offset);

create temporary table _task_parent_mappings as
with
  maxdepth as (
	select t.child_id, max(t.depth) as depth
    from _todo_closures t
    group by t.child_id
  )
select tc.parent_id, tc.child_id from maxdepth m
  join _todo_closures tc using (child_id, depth);

-- -- TODO this will be more useful if I can also link it easily with the
-- -- toplevel headline
-- drop table if exists viz.project_parent_headlines;
-- create table viz.project_parent_headlines as
-- select distinct
--   h.file_path,
--   h.headline_offset,
--   h.keyword,
--   h.headline_text
--   from _todo_closures tc
--   join headlines h
--   on tc.file_path = h.file_path and tc.parent_offset = h.headline_offset
--   where
--     not exists
--       (select * from _iterator_headlines i
--         where
--          i.file_path = h.file_path
--          and i.headline_offset = h.headline_offset)
--     and not exists
--       (select * from viz.project_toplevel_headlines t
--         where
--           t.file_path = h.file_path
--           and t.headline_offset = h.headline_offset);


create temporary table _atomic_tasks as
select * from headlines h
  where
    not h.keyword is NULL
    and not exists (select * from _project_task_headlines pt
      where pt.headline_id = h.headline_id)
    -- and not exists (select * from viz.project_parent_headlines pp
    --   where pp.headline_id = h.headline_id)
    -- and not exists (select * from viz.project_toplevel_headlines pl
    --   where pl.headline_id = h.headline_id)
    and not exists (select * from _task_parent_mappings m
      where m.parent_id = h.headline_id)
    and not exists (select * from _repeaters r
      where r.headline_id = h.headline_id)
    and not exists (select * from _iterator_headlines i
      where i.headline_id = h.headline_id);

create temporary table _iterator_tasks as
select * from headlines h
  where
    not h.keyword is NULL
    and exists (select * from _iterator_headlines i
      where i.headline_id = h.headline_id)
    and not exists (select * from _task_parent_mappings m
      where m.parent_id = h.headline_id);

-- drop table if exists viz.atomic_tasks;
-- create table viz.atomic_tasks as
-- select
--   a.*,
--   cs.clock_sum,
--   to_timestamp(s.time_start) as scheduled_time,
--   to_timestamp(d.time_start) as deadline_time,
--   to_timestamp(c.time_start) as closed_time,
--   ct.tag as category,
--   xt.tag as context,
--   rt.tags as resources,
--   t.tags as tags,
--   cr.created_timestamp,
--   (ih.tag is not NULL) as incubated
--   from _atomic_tasks a
--   left join _clock_sums cs using (headline_offset, file_path)
--   left join _scheduled_timestamps s using (file_path, headline_offset)
--   left join _deadline_timestamps d using (file_path, headline_offset)
--   left join _closed_timestamps c using (file_path, headline_offset)
--   left join _category_tags ct using (file_path, headline_offset)
--   left join _context_tags xt using (file_path, headline_offset)
--   left join _resource_tags rt using (file_path, headline_offset)
--   left join _other_tags t using (file_path, headline_offset)
--   left join _created_timestamps cr using (file_path, headline_offset)
--   left join _incubated_headlines ih using (file_path, headline_offset);

-- TODO this doesn't have iterators (yet)
drop table if exists viz.all_tasks;
create table viz.all_tasks as
with
  all_tasks as (
    select
      r.headline_id,
      r.state_new as keyword,
      r.partitioned_clock_start as clock_start,
      r.partitioned_clock_end as clock_end,
      r.partitioned_clock_sum as clock_sum,
      r.closed_timestamp,
      'repeater' as task_type
      from _repeater_headlines r
    union all
    select
      h.headline_id,
      h.state_new as keyword,
      h.partitioned_clock_start as clock_start,
      h.partitioned_clock_end as clock_end,
      h.partitioned_clock_sum as clock_sum,
      h.closed_timestamp,
      'habit' as task_type
      from _habit_headlines h
    union all
    -- TODO this is redundant to have 'tasks' made twice from different sources
    select
      a.headline_id,
      a.keyword,
      cs.clock_start,
      cs.clock_end,
      cs.clock_sum,
      to_timestamp(c.time_start) as closed_timestamp,
      'atomic' as task_type
      from _atomic_tasks a
      left join _clock_sums cs using (headline_id)
      left join _closed_timestamps c using (headline_id)
    union all
    select
      p.headline_id,
      p.keyword,
      cs.clock_start,
      cs.clock_end,
      cs.clock_sum,
      to_timestamp(c.time_start) as closed_timestamp,
      'project' as task_type
      from _project_task_headlines p
      left join _clock_sums cs using (headline_id)
      left join _closed_timestamps c using (headline_id)
    union all
    select
      i.headline_id,
      i.keyword,
      cs.clock_start,
      cs.clock_end,
      cs.clock_sum,
      to_timestamp(c.time_start) as closed_timestamp,
      'iterator' as task_type
      from _iterator_tasks i
      left join _clock_sums cs using (headline_id)
      left join _closed_timestamps c using (headline_id)
  )
select
  f.file_path,
  a.*,
  tm.parent_id as project_parent_id,
  to_timestamp(s.time_start) as scheduled_timestamp,
  to_timestamp(d.time_start) as deadline_timestamp,
  h.headline_text,
  h.effort,
  h.priority,
  h.is_archived,
  h.is_commented,
  h.content,
  ct.tag as category,
  xt.tag as context,
  rt.tags as resources,
  t.tags as tags,
  cr.created_timestamp,
  (ih.tag is not NULL) as incubated
  from all_tasks a
  join headlines h using (headline_id)
  join file_metadata f using (outline_hash)
  left join _scheduled_timestamps s using (headline_id)
  left join _deadline_timestamps d using (headline_id)
  left join _category_tags ct using (headline_id)
  left join _context_tags xt using (headline_id)
  left join _resource_tags rt using (headline_id)
  left join _other_tags t using (headline_id)
  left join _created_timestamps cr using (headline_id)
  left join _incubated_headlines ih using (headline_id)
  left join _task_parent_mappings tm on tm.child_id = h.headline_id
  order by a.headline_id, a.closed_timestamp desc;


drop table if exists viz.sleep_length;
create table viz.sleep_length as
with
  tmp as (
    select distinct
    to_localtime(clock_start) as time_start_clock,
    clock_sum,
    clock_start,
    clock_end  
    from viz.all_tasks
    where
      headline_text = 'sleep')
  select distinct
    clock_start as sleep_timestamp,
    clock_sum / 60.0 as sleep_hours,
    time_start_clock as sleep_start_clock,
    -- day of week that sleep starts; subtract 12 hours off timestamp to count
    -- bedtime after midnight as starting on the previous day
    extract(dow from (clock_start - (12||' hours')::interval) at time zone 'US/Eastern')
      as sleep_start_day,
    -- offset from target bedtime start (assume target bedtime is 23:45)
    mod((extract(hour from time_start_clock) * 60
		 + extract(minute from time_start_clock) + 15 + 720)::bigint,
		1440) / 1440.0 * 24 - 12 as sleep_start_offset from tmp;

drop table if exists viz.check_email_2;
create table viz.check_email_2 as
-- ASSUME that the repeater has the correct time of day, but may be offset
-- by a multiple of 7 days
select
  clock_start,
  extract(epoch from clock_start::time - scheduled_timestamp::time)/60 as offset_minutes
  from viz.all_tasks
where
  headline_text = 'check email 2'
order by clock_start desc;

end

$$;
