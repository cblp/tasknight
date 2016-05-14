echo "
    CREATE DATABASE tasknight_test;
    CREATE USER tasknight WITH password 'tasknight';
    GRANT ALL privileges ON DATABASE tasknight_test TO tasknight;
" | sudo -u postgres psql
