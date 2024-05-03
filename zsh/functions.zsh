kill_process_by_port() {
    if [[ -n "$1" ]]; then
        local port="$1"
        local process_id
        process_id=$(lsof -ti :$port)
        
        if [[ -n "$process_id" ]]; then
            kill -9 "$process_id"
            echo "Killed process with PID $process_id using port $port."
        else
            echo "No process found using port $port."
        fi
    else
        echo "Usage: kill_process_by_port <port>"
    fi
}
