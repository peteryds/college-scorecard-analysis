# Video Audio Normalizer - Docker Deployment

This guide explains how to build and run the Video Audio Normalizer application using Docker.

## Prerequisites

- Docker installed (version 20.10 or higher)
- Docker Compose (optional, but recommended)

## Quick Start

### Using Docker Compose (Recommended)

1. **Build and start the container:**
   ```bash
   docker-compose up -d
   ```

2. **Access the application:**
   Open your browser and navigate to: `http://localhost:8501`

3. **Stop the container:**
   ```bash
   docker-compose down
   ```

### Using Docker CLI

1. **Build the image:**
   ```bash
   docker build -t video-audio-normalizer .
   ```

2. **Run the container:**
   ```bash
   docker run -d \
     --name video-normalizer \
     -p 8501:8501 \
     -v $(pwd)/normalized_videos:/app/normalized_videos \
     -v $(pwd)/downloads:/app/downloads \
     video-audio-normalizer
   ```

3. **Access the application:**
   Open your browser and navigate to: `http://localhost:8501`

4. **Stop the container:**
   ```bash
   docker stop video-normalizer
   docker rm video-normalizer
   ```

## File Structure

```
.
├── Dockerfile                    # Docker build instructions
├── docker-compose.yml            # Docker Compose configuration
├── .dockerignore                 # Files to exclude from build
├── requirements.txt              # Python dependencies
├── packages.txt                  # System packages (ffmpeg)
├── app.py                        # Streamlit web application
├── video_audio_normalizer.py    # CLI script
├── normalized_videos/            # Output directory (created automatically)
└── downloads/                    # Temporary downloads (created automatically)
```

## Configuration

### Environment Variables

You can customize the application by setting environment variables in `docker-compose.yml`:

```yaml
environment:
  - PYTHONUNBUFFERED=1
  - MAX_RETRIES=3
  - DOWNLOAD_TIMEOUT=300
  - PROCESS_TIMEOUT=600
```

### Volume Mounts

The default setup mounts two directories:
- `./normalized_videos` - Stores processed videos
- `./downloads` - Temporary storage for downloads

To add your own video directory:
```yaml
volumes:
  - ./my-videos:/app/videos:ro
```

### Resource Limits

Adjust CPU and memory limits in `docker-compose.yml`:
```yaml
deploy:
  resources:
    limits:
      cpus: '4'          # Maximum CPU cores
      memory: 4G         # Maximum memory
    reservations:
      cpus: '2'          # Reserved CPU cores
      memory: 2G         # Reserved memory
```

## Advanced Usage

### View Container Logs

```bash
# Docker Compose
docker-compose logs -f

# Docker CLI
docker logs -f video-normalizer
```

### Execute Commands Inside Container

```bash
# Access container shell
docker exec -it video-normalizer bash

# Run CLI script directly
docker exec -it video-normalizer python video_audio_normalizer.py video.mp4
```

### Rebuild After Changes

```bash
# Docker Compose
docker-compose build --no-cache
docker-compose up -d

# Docker CLI
docker build --no-cache -t video-audio-normalizer .
```

## Troubleshooting

### Port Already in Use

If port 8501 is already in use, change it in `docker-compose.yml`:
```yaml
ports:
  - "8502:8501"  # Maps host port 8502 to container port 8501
```

### Permission Issues

If you encounter permission issues with mounted volumes:
```bash
# Linux/Mac
sudo chown -R $(id -u):$(id -g) normalized_videos downloads

# Or run container with your user ID
docker run --user $(id -u):$(id -g) ...
```

### Memory Issues

Large video processing requires significant memory. Increase limits:
```yaml
deploy:
  resources:
    limits:
      memory: 8G
```

### FFmpeg Not Found

The Dockerfile installs FFmpeg automatically. If issues persist:
```bash
# Verify FFmpeg installation
docker exec video-normalizer ffmpeg -version
```

## Health Check

The container includes a health check that monitors the application:
```bash
# Check container health
docker ps

# Manual health check
curl http://localhost:8501/_stcore/health
```

## Production Deployment

### Using a Reverse Proxy (Nginx)

```nginx
server {
    listen 80;
    server_name video-normalizer.example.com;

    location / {
        proxy_pass http://localhost:8501;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### Environment-Specific Builds

Create different compose files for different environments:

**docker-compose.dev.yml**
```yaml
version: '3.8'
services:
  video-normalizer:
    build:
      context: .
    ports:
      - "8501:8501"
    volumes:
      - .:/app
    environment:
      - DEBUG=true
```

**docker-compose.prod.yml**
```yaml
version: '3.8'
services:
  video-normalizer:
    image: your-registry/video-normalizer:latest
    ports:
      - "8501:8501"
    restart: always
    deploy:
      replicas: 2
```

## Backup and Restore

### Backup Processed Videos

```bash
# Create backup
tar -czf normalized_backup_$(date +%Y%m%d).tar.gz normalized_videos/

# Restore backup
tar -xzf normalized_backup_20250109.tar.gz
```

## Maintenance

### Cleanup

```bash
# Remove stopped containers
docker container prune

# Remove unused images
docker image prune

# Remove all unused data (careful!)
docker system prune -a --volumes
```

### Update Application

```bash
# Pull latest code
git pull

# Rebuild and restart
docker-compose up -d --build
```

## Support

For issues or questions:
- Check container logs: `docker-compose logs`
- Verify FFmpeg: `docker exec video-normalizer ffmpeg -version`
- Test Streamlit: `docker exec video-normalizer streamlit --version`

## License

See main application documentation for license information.

