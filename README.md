# 🌱 Sustainable Fashion Credits

A blockchain-based smart contract for tracking and verifying sustainable fashion materials, enabling brands to issue tokenized credits and consumers to verify ethical sourcing before purchasing.

## 🎯 Overview

This Clarity smart contract enables fashion brands to issue tokenized credits proving their use of recycled, organic, fair-trade, or sustainable materials. Consumers can verify these credits before making purchases, creating transparency in the fashion supply chain.

## ✨ Features

- 🏭 **Brand Registration**: Fashion brands can register and get verified
- 🎫 **Credit Issuance**: Verified brands can issue credits for sustainable materials
- 🔍 **Material Verification**: Consumers can verify product sustainability
- 💰 **Credit Transfer**: Credits can be transferred between accounts
- 🛒 **Purchase Verification**: Track verified purchases with sustainability proof
- ♻️ **Credit Redemption**: Redeem credits when purchasing verified products

## 🧩 Contract Functions

### Brand Management
- `register-brand(name)` - Register a new fashion brand
- `verify-brand(brand-id)` - Verify a brand (owner only)
- `get-brand-info(brand-id)` - Get brand information
- `get-brand-by-owner(owner)` - Get brand owned by principal

### Credit Management
- `issue-credit(material-type, amount, product-hash)` - Issue sustainability credits
- `transfer-credits(to, brand-id, material-type, amount)` - Transfer credits
- `redeem-credit(credit-id)` - Redeem credits for purchases
- `get-credit-info(credit-id)` - Get credit details
- `get-credit-balance(owner, brand-id, material-type)` - Check credit balance

### Verification
- `verify-product-sustainability(credit-id)` - Verify product sustainability
- `verify-before-purchase(credit-id)` - Verify before purchasing
- `get-purchase-verification(buyer, credit-id)` - Check purchase verification

## 📋 Supported Material Types

- `"recycled"` - Recycled materials
- `"organic"` - Organic materials
- `"fair-trade"` - Fair trade certified
- `"sustainable"` - General sustainable materials

## 🚀 Usage Examples

### Register a Brand
```clarity
(contract-call? .sustainable-fashion-credits register-brand "EcoFashion Co")
```

### Issue Credits
```clarity
(contract-call? .sustainable-fashion-credits issue-credit "recycled" u100 "abc123def456")
```

### Verify Product
```clarity
(contract-call? .sustainable-fashion-credits verify-product-sustainability u1)
```

### Transfer Credits
```clarity
(contract-call? .sustainable-fashion-credits transfer-credits 'SP1234... u1 "organic" u50)
```

## 🛠️ Development

### Prerequisites
- [Clarinet](https://docs.hiro.co/clarinet/getting-started)
- Node.js for testing

### Setup
```bash
git clone [repository-url]
cd sustainable-fashion-credits
clarinet check
```

### Testing
```bash
npm install
npm test
```

## 🔐 Security Features

- Owner-only brand verification
- Input validation for material types
- Balance checks for transfers
- Prevent double redemption
- Principal-based access control

## 📊 Contract Data

The contract maintains several data structures:
- **brands**: Brand registration info
- **credits**: Individual credit records  
- **credit-balances**: User credit balances by type
- **verified-purchases**: Purchase verification records

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests with `clarinet check`
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---

*Building a more sustainable fashion industry, one credit at a time* 🌍
