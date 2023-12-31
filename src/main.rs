use warp::Filter;
use serde::{Serialize, Deserialize};

#[derive(Deserialize)]
struct PingRequest {
    browser: String,
    platform: String,
    message: String
}

#[derive(Serialize)]
struct RepoResponse {
    repos: Vec<String>
}

async fn get_repos() -> Result<impl warp::Reply, warp::Rejection> {
    // For simplicity, let's say we are returning a static post
    let repos = RepoResponse {
        repos: vec!["Test Repo 1", "Test Repo 2"]
            .iter()
            .map(|&s| String::from(s))
            .collect()
    };
    return Ok(warp::reply::json(&repos));
    
}

#[tokio::main]
async fn main() {
    // GET /hello/warp => 200 OK with body "Hello, warp!"
    let route = warp::path("repos")
        .and(warp::get())
        .and_then(get_repos);

    warp::serve(route)
        .run(([127, 0, 0, 1], 3030))
        .await;
}