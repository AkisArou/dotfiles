use sense_model::{
    CompletionItem, CompletionRequest, Diagnostic, DocumentationState, Generation, ItemId, Preview,
    RequestId, SessionId, SignatureHelp, SourceId,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ProtocolVersion {
    pub major: u16,
    pub minor: u16,
}

impl ProtocolVersion {
    pub const CURRENT: Self = Self { major: 1, minor: 0 };

    #[must_use]
    pub const fn is_compatible_with(self, other: Self) -> bool {
        self.major == other.major
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PeerRole {
    ZleClient,
    CompletionWorker,
    Adapter,
    Cli,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ZshIdentity {
    pub executable: String,
    pub version: String,
    pub patchlevel: Option<String>,
    pub native_abi_key: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ClientHello {
    pub protocol: ProtocolVersion,
    pub client_version: String,
    pub role: PeerRole,
    pub process_id: u32,
    pub zsh: Option<ZshIdentity>,
    /// Workers attach to a session created by the ZLE client.
    pub attach_session: Option<SessionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ServerHello {
    pub protocol: ProtocolVersion,
    pub daemon_version: String,
    pub session_id: SessionId,
    pub max_frame_bytes: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CandidateBatch {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    pub source: SourceId,
    pub items: Vec<CompletionItem>,
    pub is_final: bool,
    pub is_incomplete: bool,
}

/// A daemon-ranked, merged view for presentation by the ZLE client.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CandidateView {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    /// Monotonically increases as sources stream updates for this request.
    pub revision: u64,
    pub items: Vec<CompletionItem>,
    pub selected_index: Option<u32>,
    pub matched_before_limit: u32,
    pub sources_pending: Vec<SourceId>,
    pub is_final: bool,
    pub is_incomplete: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolveRequest {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    pub item_id: ItemId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelectionRequest {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    pub item_id: ItemId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum ClientMessage {
    Hello(ClientHello),
    Complete(CompletionRequest),
    PublishCandidates(CandidateBatch),
    Cancel {
        session_id: SessionId,
        request_id: RequestId,
        generation: Generation,
    },
    Select(SelectionRequest),
    Resolve(ResolveRequest),
    Ping {
        nonce: u64,
    },
    Goodbye,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum ServerMessage {
    Welcome(ServerHello),
    CompletionRequested(CompletionRequest),
    RequestCancelled {
        request_id: RequestId,
        generation: Generation,
    },
    SelectionRequested(SelectionRequest),
    ResolveRequested(ResolveRequest),
    RequestStarted {
        request_id: RequestId,
        generation: Generation,
    },
    Candidates(CandidateBatch),
    CandidateView(CandidateView),
    RequestFinished {
        request_id: RequestId,
        generation: Generation,
        cancelled: bool,
    },
    Documentation {
        request_id: RequestId,
        generation: Generation,
        item_id: ItemId,
        documentation: DocumentationState,
    },
    Signature {
        request_id: RequestId,
        generation: Generation,
        signature: SignatureHelp,
    },
    Diagnostics {
        request_id: RequestId,
        generation: Generation,
        diagnostics: Vec<Diagnostic>,
    },
    Preview {
        request_id: RequestId,
        generation: Generation,
        preview: Preview,
    },
    SelectionAccepted(SelectionRequest),
    Status {
        message: String,
    },
    Pong {
        nonce: u64,
    },
    Error {
        code: String,
        message: String,
        request_id: Option<RequestId>,
    },
}
