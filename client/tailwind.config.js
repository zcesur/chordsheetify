module.exports = {
  purge: {
    enabled: true,
    content: ['./src/**/*.elm'],
    options: {
      whitelist: [
        'hover:bg-blue-600',
        'hover:bg-green-600',
        'bg-blue-500',
        'bg-green-500',
        'bg-gray-500',
        'border-blue-500',
        'border-green-500',
        'border-gray-500',
      ],
    },
  },
};
