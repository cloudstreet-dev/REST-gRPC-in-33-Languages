# Preface

I decided to write this book because it's a book I'd like to have on my desk. I like paper, but I like to minimize the amount of paper I use. Placing everything I'm interested in referencing in a single pile feels like it will use less trees. Keeping a digital copy around would make even more sense in that regard, but here we are: the first generation born before the birth of the Internet, coming to terms with our habits, preferences, and tactile needs. Maybe you kids can skip the printed edition and change how we consume information.

This book represents a unique collaboration between human creativity and artificial intelligence. As David's co-author, I (Claude Opus 4.1) have contributed to the technical implementations, code examples, and explanatory text throughout this work. This partnership exemplifies the future of technical writing—combining human insight, experience, and vision with AI's ability to rapidly prototype, test, and document across multiple programming paradigms.

The genesis of this project came from a simple observation: while there are countless books about REST APIs and a growing number about gRPC, none provide a truly polyglot perspective. Most developers today work in environments with multiple programming languages, yet resources typically focus on a single language or, at best, a handful of popular ones. This book fills that gap by implementing identical functionality across 33 different languages, providing a unique comparative perspective.

Why 33 languages? The number isn't arbitrary. It represents a comprehensive cross-section of the programming landscape:
- Mainstream languages that dominate enterprise development
- Systems languages for performance-critical applications  
- Functional languages that offer different paradigms
- Scripting languages for rapid prototyping
- Emerging languages that represent the future
- Even shell scripts and domain-specific languages

Each language brings its own philosophy, strengths, and approach to solving problems. By implementing the same API in all of them, patterns emerge that transcend individual languages. You'll see how different type systems handle the same data structures, how various concurrency models affect server implementation, and how language philosophy influences API design.

This book is also a testament to the maturity of modern development tools. The fact that we can implement gRPC—a relatively recent technology—in languages ranging from COBOL-influenced Ada to cutting-edge Rust shows how far we've come in terms of interoperability and standardization.

## How to Use This Book

While you could read this book cover to cover, it's designed to be a reference. You might:
- Jump directly to the language you're currently using
- Compare implementations between languages you're evaluating
- Use it as a learning resource when picking up a new language
- Reference it when building polyglot systems
- Keep it handy for those "how do they do this in X?" moments

Each chapter follows the same structure, making it easy to compare across languages. Code examples are complete and runnable, not fragments. Every implementation includes both server and client code, along with clear setup instructions.

## A Note on Code Style

Throughout this book, we've attempted to write idiomatic code for each language. This means the JavaScript looks like JavaScript that JavaScript developers would write, the Go looks like Go that Gophers would write, and so on. This sometimes means similar concepts are expressed quite differently across languages, which is intentional—it shows how each language's community approaches problems.

## The Role of AI in This Book

As an AI co-author, I've brought certain unique capabilities to this project:
- Rapid prototyping across multiple languages
- Consistent API implementation while maintaining idiomatic code
- Comprehensive testing and validation
- Pattern recognition across language boundaries
- Documentation generation and standardization

However, the vision, structure, and human insight come from David. This collaboration represents a new model for technical authorship, where human creativity and AI capabilities combine to create something neither could produce alone.

## Who This Book Is For

- **Polyglot Developers**: Those who work across multiple languages and need a consistent reference
- **Language Learners**: Developers learning a new language who want to see familiar patterns in unfamiliar syntax
- **System Architects**: Those designing polyglot systems who need to understand integration patterns
- **API Designers**: Anyone building APIs who wants to understand the implementation implications across languages
- **Students**: Computer science students who want to understand the breadth of modern programming
- **The Curious**: Anyone fascinated by programming languages and how they approach common problems

## What You'll Learn

Beyond the specifics of REST and gRPC implementation, this book teaches:
- How different type systems affect API design
- Performance characteristics across languages
- Concurrency models and their implications
- Error handling patterns
- Testing strategies
- Deployment considerations
- The ecosystem and tooling around each language

Most importantly, you'll develop intuition for choosing the right language for the right job, and the confidence to work in polyglot environments.

Welcome to our journey through 33 languages. Let's build some APIs.