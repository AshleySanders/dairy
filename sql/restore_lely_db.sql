RESTORE FILELISTONLY
FROM DISK = '/var/opt/mssql/backup/GD_Lely.bak';

RESTORE DATABASE GD_Lely
FROM DISK = '/var/opt/mssql/backup/GD_Lely.bak'
WITH MOVE 'LELY_Data' TO '/var/opt/mssql/data/GD_Lely_Data.mdf',
     MOVE 'LELY_Log' TO '/var/opt/mssql/data/GD_Lely_Log.ldf',
     REPLACE;
